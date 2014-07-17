{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module MPD.Core
  (
    Command(..)
  , run
  , runWith

  , Folder
  , liftFold
  , id_
  ) where

import MPD.CommandStr (CommandStr, render)

import Control.Applicative (Applicative(..), (<$>), (<|>), empty)
import Control.Arrow ((***), second)
import Control.Monad (ap, unless)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString      as SB
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified System.IO as IO
import qualified System.IO.Error as IO
import Network (HostName, PortID(..), connectTo)

------------------------------------------------------------------------

data Command a = Command
  { commandReq :: [CommandStr]
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

instance Applicative Folder where
  pure  = return
  {-# INLINE pure #-}

  (<*>) = ap
  {-# INLINE (<*>) #-}

instance Monad Folder where
  return x = Folder $ \s -> (x, s)
  {-# INLINE return #-}

  Folder f >>= g = Folder $ (\(!a, r) -> runFolder (g a) r) . f
  {-# INLINE (>>=) #-}

id_ :: Folder [SB.ByteString]
id_ = liftFold id

liftFold :: ([SB.ByteString] -> a) -> Folder a
liftFold f = Folder $ (f *** drop 1) . break (== "list_OK")

------------------------------------------------------------------------

run :: Command a -> IO a
run = runWith "localhost" (PortNumber 6600)

runWith :: HostName -> PortID -> Command a -> IO a
runWith host port (Command q p) = do
  hdl <- connectTo host port
  IO.hSetNewlineMode hdl IO.noNewlineTranslation
  IO.hSetEncoding hdl IO.utf8
  IO.hSetBuffering hdl IO.LineBuffering

  helo <- SB.hGetLine hdl
  unless ("OK MPD" `SB.isPrefixOf` helo) $
    fail "runWith: host failed to identify as MPD"

  SB.hPut hdl (pack $ map render q)
  !res <- response `fmap` LB.hGetContents hdl

  _ <- IO.tryIOError (SB.hPut hdl "close\n")
  IO.hClose hdl
  either (fail . T.unpack) (return . fst . runFolder p) res

pack :: [T.Text] -> SB.ByteString
pack = SB.concat . map ((`SB.snoc` 10) . T.encodeUtf8)
     . (\q -> "command_list_ok_begin" : q ++ ["command_list_end"])

------------------------------------------------------------------------

response :: LB.ByteString -> Either T.Text [SB.ByteString]
response = step . map LB.toStrict . LB.split 10
  where
    step (hd:tl)
      | hd == "OK"               = Right []
      | "ACK" `SB.isPrefixOf` hd = Left . T.decodeUtf8 $ SB.drop 4 hd
      | otherwise                = fmap (hd :) (step tl)
    step [] = Left "premature end-of-input"
