{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module Atto (

  spec,

  -- * Core

  -- ** Parsers
  pairP, fieldP,
  boolP, intP, textP,

  -- ** Command specification interface
  Command, command,

  -- ** Command driver
  simple, run,

  -- * Wrappers

  LsEntryInfo(..),
  ping, playlistInfo, listAllInfo,

  ) where

import Core

import Control.Applicative
import Data.Functor (($>))
import Data.Function (fix)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8            as SB
import qualified Data.Attoparsec.ByteString.Char8 as A

import System.IO (BufferMode(..), hSetBuffering)
import System.IO.Error

import Test.Hspec

------------------------------------------------------------------------
-- Parser

pairP :: ByteString -> A.Parser a -> A.Parser (ByteString, a)
pairP k v = (,) <$> A.string k <* A.string ": " <*> v <* A.char '\n'

fieldP :: ByteString -> A.Parser a -> A.Parser a
fieldP k v = snd <$> pairP k v

boolP = A.char '0' $> False <|> A.char '1' $> True
textP = A.takeWhile1 (/= '\n')
intP  = A.decimal :: A.Parser Int

response :: A.Parser a -> A.Parser (Either ByteString a)
response p =
  Right <$> (p <* "list_OK\n") <|>
  Left  <$> ("ACK " *> A.takeWhile1 (/= '\n') <* A.char '\n')

------------------------------------------------------------------------
-- Specification

spec :: Spec
spec = describe "Parser" $ do

  let
    object :: A.Parser (ByteString, Int)
    object =
      (,) <$> fieldP "file" (A.takeWhile1 (/= '\n'))
          <*> fieldP "time" A.decimal

  it "pairP" $ do
    A.parseOnly (pairP "key" A.decimal) "key: 10\n"
    `shouldBe` Right ("key", 10)

  it "fieldP" $ do
    A.parseOnly (fieldP "key" A.decimal) "key: 10\n"
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
-- Wrappers

ping :: Command ()
ping = command "ping" (return ())

listAllInfo :: Command [LsEntryInfo]
listAllInfo = command "listallinfo" (A.many' lsEntryInfoP)

data LsEntryInfo
  = LsEntrySongInfo !ByteString !ByteString !Int ![(ByteString, ByteString)]
  | LsEntryDirInfo !ByteString !ByteString
  | LsEntryPListInfo !ByteString !ByteString
    deriving (Show)

lsEntryInfoP =
      (LsEntrySongInfo
       <$> fieldP "file" textP
       <*> fieldP "Last-Modified" textP
       <*> fieldP "Time" intP
       <*> A.many' songTagP
      )
  <|> (LsEntryDirInfo
       <$> fieldP "directory" textP
       <*> fieldP "Last-Modified" textP
      )
  <|> (LsEntryPListInfo
       <$> fieldP "playlist" textP
       <*> fieldP "Last-Modified" textP
      )

playlistInfo :: Command [PlaylistSongInfo]
playlistInfo = command "playlistinfo" (A.many' playlistSongInfoP)

data PlaylistSongInfo = PlaylistSongInfo
  !ByteString
  !ByteString
  ![(ByteString, ByteString)]
  !Int
  !(Maybe Int)
  !(Maybe Int)
    deriving (Show)

-- note: here, Time comes after the tags, whereas for LsEntrySongInfo,
-- Time comes BEFORE tags. Why you no consistent?

playlistSongInfoP = PlaylistSongInfo <$>
  fieldP "file" textP <*>
  fieldP "Last-Modified" textP <*>
  A.many' songTagP <*>
  fieldP "Time" intP <*>
  optional (fieldP "Pos" intP) <*>
  optional (fieldP "Id" intP)

songTagP = A.choice (map (`pairP` textP) tags) where
  tags =
    [
      "Album"
    , "Artist"
    , "Date"
    , "Genre"
    , "Title"

    , "Track"
    , "Disc"

    , "Composer"
    , "Performer"

    , "AlbumArtist"
    , "AlbumArtistSort"
    , "AlbumSort"
    , "ArtistSort"

    , "MUSICBRAINZ_ARTISTID"
    , "MUSICBRAINZ_ALBUMID"
    , "MUSICBRAINZ_ALBUMARTISTID"
    , "MUSICBRAINZ_TRACKID"
    ]

------------------------------------------------------------------------
-- Commands

data Command a = Command [ByteString] (A.Parser (Either ByteString a))

command q p = Command [q] (response p)

------------------------------------------------------------------------
-- Driver

simple c = mpd $ \h -> run h c

run h (Command q p) = do
  send h q
  hSetBuffering h (BlockBuffering $ Just kBUFSIZ)
  A.parseWith (SB.hGetSome h kBUFSIZ) (p <* "OK\n") ""

kBUFSIZ = 64 :: Int

------------------------------------------------------------------------
-- Driver model

simple_model (Command q p) k = mpd $ \h -> run_model h (q, p <* "OK\n") k

run_model h (q, p) k = send h q >> process h p k

process h p k = do
  hSetBuffering h (BlockBuffering $ Just kBUFSIZ)
  start recv p k
  where recv = SB.hGetSome h kBUFSIZ

start g p k = worker g k (A.parse p "")

worker g k = fix $ \recur p -> do
  ln <- g
  case A.feed p ln of
    A.Partial p' -> recur (A.Partial p')

    A.Done "" r  -> k r
    A.Done i _   -> fail ("superflous input: " ++ show i)

    A.Fail i _ s -> fail ("failed parsing " ++ show i ++ "; " ++ s)
