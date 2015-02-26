{-# LANGUAGE OverloadedStrings #-}

module Foo where

import Control.Applicative
import Control.Monad ((>=>), (<=<), join)

import Data.Function
import Data.Semigroup

import Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as SB

import System.IO
import System.IO.Error

import Network

------------------------------------------------------------------------
-- Parser

pair :: ByteString -> Parser a -> Parser (ByteString, a)
pair k v = (,) <$> string k <* string ": " <*> v <* char '\n'

field :: ByteString -> Parser a -> Parser a
field k v = snd <$> pair k v

response :: Parser a -> Parser (Either ByteString a)
response p =
  Right <$> (p <* "list_OK\n") <|>
  Left  <$> ("ACK " *> takeWhile1 (/= '\n') <* char '\n')

object :: Parser (ByteString, Int)
object =
  (,) <$> field "file" (takeWhile1 (/= '\n'))
      <*> field "time" decimal

------------------------------------------------------------------------
-- Dummy input

dummy1, dummy2, dummy3 :: ByteString
dummy1 = "file: foo.mp3\ntime: 420\nlist_OK\n"
dummy2 = "file: bar.mp3\ntime: 120\nlist_OK\n"
dummy3 = "ACK protocol error\n"

test1 = parse (response object) dummy1
test2 = parse (response object) dummy3
test3 = parse ((,) <$> response object <*> response object) (dummy1 <> dummy2)
test4 = parse ((,) <$> response object <*> response object) dummy1
test5 = parse ((,) <$> response object <*> response object) (dummy1 <> dummy3)
test6 = parse (response object) (dummy1 <> dummy2)
test7 = parse (response object) (dummy1 <> dummy3)

------------------------------------------------------------------------
-- Commands

connect = do
  h <- connectTo "localhost" (PortNumber 6600)
  l <- SB.hGetLine h
  case SB.splitAt 7 l of
    ("OK MPD ", v) -> return (h, v)
    _              -> fail "invalid host"

pack = SB.unlines
     . ("command_list_ok_begin" :)
     . (++ ["command_list_end"])
     . filter (not . SB.null)

run q p k = do
  (h, _) <- connect
  r <- runWith h q p k
  hClose h
  return r

runWith h q p k = do
  SB.hPut h (pack q)
  process h (p <* "OK\n") k

recv h = SB.hGetSome h 64

process h p k = start (recv h) p k

start g p k =
  worker g k (parse p "")

worker g k = fix $ \recur p -> do
  print p
  ln <- g
  case feed p ln of
    Partial p' -> recur (Partial p')
    Done "" r  -> k r
    Done i _   -> fail ("superflous input: " ++ show i)
    Fail i _ s -> fail ("failed parsing " ++ show i ++ "; " ++ s)
