{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}

module MPD.Commands.ParserSpec (spec) where

import MPD.Core
import MPD.Commands.Parser

import Control.Applicative
import qualified Data.Attoparsec.ByteString as A
import Data.Monoid (mconcat, (<>))
import Test.Hspec
import Test.Hspec.Expectations.Contrib

------------------------------------------------------------------------
-- Main specification.

spec :: Spec
spec = do

  it "dateP" $
    A.parseOnly dateP "2014-05-16T17:33:26Z"
    `shouldSatisfy` isRight

  it "volumeP" $
    A.parseOnly volumeP "100"
    `shouldSatisfy` isRight

  it "pathP" $
    A.parseOnly pathP "foo/bar.mp3"
    `shouldSatisfy` isRight

  it "timeElapsedP" $
    A.parseOnly timeElapsedP "10:120"
    `shouldSatisfy` isRight

  it "audioP" $
    A.parseOnly audioP "4800:2:24"
    `shouldSatisfy` isRight

  it "songInfo 1" $
    songInfo `shouldAccept` songInfo1

  it "songInfo 2" $
    songInfo `shouldAccept` songInfo2

  it "songInfo 3" $
    songInfo `shouldAccept` songInfo3

  it "songInfo 4" $
    songInfo `shouldAccept` songInfo4

  it "songInfo 1 + 2 + 3 + 4" $
    many songInfo
    `shouldAccept` mconcat [ songInfo1, songInfo2, songInfo3, songInfo4 ]

------------------------------------------------------------------------
-- Internal helpers.

p `shouldAccept` i = parse p i `shouldSatisfy` isRight

------------------------------------------------------------------------
-- Example data

songInfo1 = [
    "file: foo.mp3"
  , "Last-Modified: 2014-05-16T17:33:26Z"
  , "Time: 320"
  ]

songInfo2 = songInfo1 <> [
    "Pos: 0"
  , "Id: 0"
  ]

songInfo3 = songInfo1 <> [
    "Artist: FooArtist"
  , "AlbumArtist: FooArtist"
  , "Title: FooTitle"
  , "Album: FooAlbum"
  , "Track: 04/11"
  , "Date: 1998"
  , "Genre: Electronic"
  ]

songInfo4 = songInfo3 <> [
    "Pos: 0"
  , "Id: 0"
  ]
