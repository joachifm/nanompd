{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns      #-}

module MPD
  ( Command(..)
  , run

  , Folder
  , liftFold
  , id_

  , ping
  , status
  , currentsong
  , listallinfo
  ) where

import Control.Applicative (Applicative(..))
import Control.Arrow (second)
import Control.Monad (Monad(..), ap)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString      as SB
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified System.IO as IO
import qualified System.IO.Error as IO
import Network

------------------------------------------------------------------------

ping :: Command ()
ping = Command ["ping"] (return ())

currentsong :: Command [SB.ByteString]
currentsong = Command ["currentsong"] id_

status :: Command [SB.ByteString]
status = Command ["status"] id_

listallinfo :: Command [SB.ByteString]
listallinfo = Command ["listallinfo"] id_

------------------------------------------------------------------------

newtype Folder a = Folder {
  runFolder :: [SB.ByteString] -> (a, [SB.ByteString])
  } deriving (Functor)

instance Monad Folder where
  return x = Folder $ \_ -> (x, [])
  Folder f >>= g = Folder $ \s ->
    let (a, r) = {-# SCC "Folder/f" #-} f s in
    {-# SCC "Folder/>>=" #-} runFolder (g a) r

id_ :: Folder [SB.ByteString]
id_ = liftFold id

liftFold :: ([SB.ByteString] -> a) -> Folder a
liftFold f = Folder $ \s ->
  let (hd, tl) = {-# SCC "liftFold/break" #-} break (== "list_OK") s
  in ({-# SCC "liftFold/f" #-} f hd, drop 1 tl)

instance Applicative Folder where
  pure  = return
  (<*>) = ap

------------------------------------------------------------------------

data Command a = Command
  { commandReq :: [T.Text]
  , commandRes :: Folder a
  } deriving (Functor)

instance Applicative Command where
  pure x = Command [] (pure x)
  Command q1 p1 <*> Command q2 p2 = Command (q1 ++ q2) (p1 <*> p2)

------------------------------------------------------------------------

run :: Command a -> IO a
run (Command q p) = do
  hdl <- connectTo "localhost" (PortNumber 6600)
  IO.hSetNewlineMode hdl IO.noNewlineTranslation
  IO.hSetEncoding hdl IO.utf8
  _ <- SB.hGetLine hdl

  SB.hPut hdl (pack q) >> IO.hFlush hdl
  !res <- response `fmap` LB.hGetContents hdl

  _ <- IO.tryIOError (SB.hPut hdl "close\n" >> IO.hFlush hdl)
  IO.hClose hdl

  case res of
    Left err -> fail ("run: invalid response: " ++ err)
    Right (code, body)
      | Right () <- code -> return . fst $! runFolder p (map LB.toStrict body)
      | Left ack <- code -> fail (show ack)

pack :: [T.Text] -> SB.ByteString
pack = SB.concat . map ((`SB.snoc` 10) . T.encodeUtf8)
     . (\q -> "command_list_ok_begin" : q ++ ["command_list_end"])

------------------------------------------------------------------------

response
  :: LB.ByteString
  -> Either String (Either LB.ByteString (), [LB.ByteString])
response = step . ({-# SCC "response/split" #-} LB.split 10)

step
  :: [LB.ByteString]
  -> Either String (Either LB.ByteString (), [LB.ByteString])
step [] = Left "response: premature end of input"
step (hd:tl)
  | Just code <- {-# SCC "step/end" #-} end hd = Right (code, [])
  | otherwise = {-# SCC "step/recur" #-} fmap (second (hd :)) (step tl)

end :: LB.ByteString -> Maybe (Either LB.ByteString ())
end x | x == "OK"               = Just $ Right ()
      | "ACK" `LB.isPrefixOf` x = Just $ Left $ LB.drop 4 x
      | otherwise               = Nothing
