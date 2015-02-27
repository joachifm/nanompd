{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module Atto where

import Core

import Control.Monad.Trans.Except (ExceptT(..))

import Control.Applicative
import Data.Functor
import Data.Function
import Data.Semigroup

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8            as SB
import qualified Data.Attoparsec.ByteString.Char8 as A

import System.IO (Handle)
import System.IO.Error

import Network

import Test.Hspec

------------------------------------------------------------------------
-- Parser

pair :: ByteString -> A.Parser a -> A.Parser (ByteString, a)
pair k v = (,) <$> A.string k <* A.string ": " <*> v <* A.char '\n'

field :: ByteString -> A.Parser a -> A.Parser a
field k v = snd <$> pair k v

bool = A.char '0' $> False <|> A.char '1' $> True
text = A.takeWhile1 (/= '\n')
int  = (A.decimal :: A.Parser Int)

response :: A.Parser a -> A.Parser (Either ByteString a)
response p =
  Right <$> (p <* "list_OK\n") <|>
  Left  <$> ("ACK " *> A.takeWhile1 (/= '\n') <* A.char '\n')

------------------------------------------------------------------------
-- Dummy input

object :: A.Parser (ByteString, Int)
object =
  (,) <$> field "file" (A.takeWhile1 (/= '\n'))
      <*> field "time" A.decimal

spec :: Spec
spec = describe "Parser" $ do

  it "pair" $ do
    A.parseOnly (pair "key" A.decimal) "key: 10\n"
    `shouldBe` Right ("key", 10)

  it "field" $ do
    A.parseOnly (field "key" A.decimal) "key: 10\n"
    `shouldBe` Right 10

  it "response/single OK" $ do
    A.parseOnly (response object) "file: foo.mp3\ntime: 420\nlist_OK\n"
    `shouldBe` Right (Right ("foo.mp3", 420))

  it "response/single ACK" $ do
    A.parseOnly (response object) "ACK protocol error\n"
    `shouldBe` Right (Left "protocol error")

  it "response/single, left-overs" $ do
    A.parseOnly (response object)
                "file: foo.mp3\ntime: 420\nlist_OK\nfile: bar.mp3\ntime: 120\nlist_OK\n"
    `shouldBe` Right (Right ("foo.mp3", 420))

  it "response/combined, no left-overs" $ do
    A.parseOnly ((,) <$> response object <*> response object)
                "file: foo.mp3\ntime: 420\nlist_OK\nfile: bar.mp3\ntime: 120\nlist_OK\n"
    `shouldBe` Right (Right ("foo.mp3", 420), Right ("bar.mp3", 120))

  it "response/combined, insufficient input" $ do
    A.parseOnly ((,) <$> response object <*> response object)
                "file: foo.mp3\ntime: 420\nlist_OK\n"
    `shouldBe` Left "not enough input"

------------------------------------------------------------------------
-- Commands

ping = command "ping" (return ())

playlistinfo = command "playlistinfo" playlistinfoP

playlistinfoP = A.many' $ (,,,,,) <$>
  field "file" text <*>
  field "Last-Modified" text <*>
  A.many' songTag <*>
  field "Time" int <*>
  optional (field "Pos" int) <*>
  optional (field "Id" int)

songTag = A.choice (map (`field` text) tags) where
  tags =
    [
      "Album"
    , "Artist"
    , "Composer"
    , "Date"
    , "Disc"
    , "Genre"
    , "Performer"
    , "Title"
    , "Track"

    , "AlbumArtist"
    , "AlbumArtistSort"
    , "AlbumSort"
    , "ArtistSort"

    , "MUSICBRAINZ_ARTISTID"
    , "MUSICBRAINZ_ALBUMID"
    , "MUSICBRAINZ_ALBUMARTISTID"
    , "MUSICBRAINZ_TRACKID"
    ]

data Command a = Command [ByteString] (A.Parser (Either ByteString a))

command q p = Command [q] (response p)

simple (Command q p) k = mpd $ \h -> runWith h (q, p <* "OK\n") k

runWith h (q, p) k = send h q >> process h p k

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
