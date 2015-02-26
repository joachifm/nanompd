{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module Atto where

import Core

import Control.Applicative
import Data.Function
import Data.Semigroup

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8            as SB
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
-- Direct

simple (q, p) k = do
  (h, _) <- connect
  r <- runWith h (q, p <* "OK\n") k
  hClose h
  return r

runWith h (q, p) k = send h q >> (process h p k)

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
