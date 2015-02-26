{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module Foo () where

import Data.Void

import Control.Monad.Trans
import Control.Monad.Identity
import Control.Monad.Trans.Free

import Control.Monad.Cont (ContT(..))
import qualified Control.Monad.Cont as Cont

import Control.Monad.Trans.Except (ExceptT(..))
import qualified Control.Monad.Trans.Except as Except

import Pipes
import qualified Pipes.Attoparsec as PA
import qualified Pipes.Parse  as PP
import qualified Pipes.Group as PG
import qualified Pipes.ByteString as PB

import Control.Applicative
import Control.Monad ((>=>), (<=<), join)

import Data.Function
import Data.Semigroup

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as SB
import qualified Data.Attoparsec.ByteString.Char8 as A

import System.IO
import System.IO.Error

import Network

------------------------------------------------------------------------
-- Parser

pair :: ByteString -> A.Parser a -> A.Parser (ByteString, a)
pair k v = (,) <$> A.string k <* A.string ": " <*> v <* A.char '\n'

field :: ByteString -> A.Parser a -> A.Parser a
field k v = snd <$> pair k v

response :: A.Parser a -> A.Parser (Either ByteString a)
response p =
  Right <$> (p <* "list_OK\n") <|>
  Left  <$> ("ACK " *> A.takeWhile1 (/= '\n') <* A.char '\n')

object :: A.Parser (ByteString, Int)
object =
  (,) <$> field "file" (A.takeWhile1 (/= '\n'))
      <*> field "time" A.decimal

------------------------------------------------------------------------
-- Dummy input

dummy1, dummy2, dummy3 :: ByteString
dummy1 = "file: foo.mp3\ntime: 420\nlist_OK\n"
dummy2 = "file: bar.mp3\ntime: 120\nlist_OK\n"
dummy3 = "ACK protocol error\n"

test1 = A.parse (response object) dummy1
test2 = A.parse (response object) dummy3
test3 = A.parse ((,) <$> response object <*> response object) (dummy1 <> dummy2)
test4 = A.parse ((,) <$> response object <*> response object) dummy1
test5 = A.parse ((,) <$> response object <*> response object) (dummy1 <> dummy3)
test6 = A.parse (response object) (dummy1 <> dummy2)
test7 = A.parse (response object) (dummy1 <> dummy3)

------------------------------------------------------------------------
-- Core

data ClientError
  = ProtocolError ByteString
  | InvalidHost HostName PortID
  | ConnError IOError
  | Custom String
    deriving (Show)

connect = do
  h <- connectTo "localhost" (PortNumber 6600)
  l <- SB.hGetLine h
  case SB.splitAt 7 l of
    ("OK MPD ", v) -> return (h, v)
    _              -> fail "invalid host"

close h = SB.hPut h "close\n" >> hClose h

send h = SB.hPut h . pack

pack = SB.unlines
     . ("command_list_ok_begin" :)
     . (++ ["command_list_end"])
     . filter (not . SB.null)

------------------------------------------------------------------------
-- Cont

command :: ByteString -> A.Parser a -> ([ByteString], A.Parser a)
command q p = ([q], p <* "list_OK\n")

simple (q, p) k = do
  (h, _) <- connect
  r <- runWith h (q, p <* "OK\n") k
  hClose h
  return r

runWith h (q, p) k = do
  SB.hPut h (pack q)
  process h p k

process h p k = start recv p k
  where recv = SB.hGetSome h 64

start g p k = worker g k (A.parse p "")

worker g k = fix $ \recur p -> do
  ln <- g
  case A.feed p ln of
    A.Partial p' -> recur (A.Partial p')
    A.Done "" r  -> k r
    A.Done i _   -> fail ("superflous input: " ++ show i)
    A.Fail i _ s -> fail ("failed parsing " ++ show i ++ "; " ++ s)

------------------------------------------------------------------------
-- Pipes

connectE :: (MonadIO m) => Effect m (Handle, ByteString)
connectE = liftIO connect

sendE :: (MonadIO m) => Handle -> [ByteString] -> Effect m ()
sendE h xs = liftIO (SB.hPut h (pack xs))

closeE :: (MonadIO m) => Handle -> Effect m ()
closeE h = liftIO (close h)

test_p_1 = runEffect $
  for (PA.parsed (object <* "list_OK\n") (yield dummy1)) (lift . print)
