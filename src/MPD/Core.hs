{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveFunctor     #-}

module MPD.Core
  (
    Command(..)
  , run
  , runWith

  , Folder
  , liftFold
  , id_
  ) where

import Control.Applicative (Applicative(..))
import Control.Arrow ((***), second)
import Control.Monad (ap, unless)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString      as SB
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified System.IO as IO
import qualified System.IO.Error as IO
import Network

------------------------------------------------------------------------

data Command a = Command
  { commandReq :: [T.Text]
  , commandRes :: Folder a
  } deriving (Functor)

instance Applicative Command where
  pure x = Command [] (pure x)
  {-# INLINE pure #-}
  Command q1 p1 <*> Command q2 p2 = Command (q1 ++ q2) (p1 <*> p2)
  {-# INLINE (<*>) #-}

------------------------------------------------------------------------

newtype Folder a = Folder {
  runFolder :: [SB.ByteString] -> (a, [SB.ByteString])
  } deriving (Functor)

instance Monad Folder where
  return x = Folder $ \s -> (x, s)
  {-# INLINE return #-}

  Folder f >>= g = Folder $ (\(a, r) -> runFolder (g a) r) . f
  {-# INLINE (>>=) #-}

id_ :: Folder [SB.ByteString]
id_ = liftFold id

liftFold :: ([SB.ByteString] -> a) -> Folder a
liftFold f = Folder $ (f *** drop 1) . break (== "list_OK")

instance Applicative Folder where
  pure  = return
  {-# INLINE pure #-}
  (<*>) = ap
  {-# INLINE (<*>) #-}

------------------------------------------------------------------------

run :: Command a -> IO a
run = runWith "localhost" 6600

runWith :: String -> Integer -> Command a -> IO a
runWith host port (Command q p) = do
  hdl <- connectTo host (PortNumber $ fromIntegral port)
  IO.hSetNewlineMode hdl IO.noNewlineTranslation
  IO.hSetEncoding hdl IO.utf8
  ver <- SB.hGetLine hdl
  unless ("OK MPD" `SB.isPrefixOf` ver) $ fail "runWith: not MPD"

  SB.hPut hdl (pack q) >> IO.hFlush hdl
  !res <- response `fmap` LB.hGetContents hdl

  _ <- IO.tryIOError (SB.hPut hdl "close\n" >> IO.hFlush hdl)
  IO.hClose hdl

  case res of
    Left err -> fail ("run: invalid response: " ++ err)
    Right (code, body) -> case code of
      Right () -> return . fst $! runFolder p (map LB.toStrict body)
      Left ack -> fail (show ack)

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
  | otherwise = {-# SCC "step/recur" #-} second (hd :) `fmap` step tl

end :: LB.ByteString -> Maybe (Either LB.ByteString ())
end x | x == "OK"               = Just $ Right ()
      | "ACK" `LB.isPrefixOf` x = Just $ Left $ LB.drop 4 x
      | otherwise               = Nothing
