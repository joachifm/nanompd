{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}

module MPD.ParserSpec (spec) where

import MPD

import Control.Applicative
import Data.Monoid (mconcat, (<>))
import Test.Hspec
import Test.Hspec.Expectations.Contrib

------------------------------------------------------------------------
-- Main specification.

spec :: Spec
spec = describe "Protocol object parsers" $ do

  it "field/single" $ do
    field "foo" intP `shouldAccept` [ "foo: 1337" ]

  it "field/compound" $ do
    ((,) <$> field "foo" intP <*> field "bar" boolP)
    `shouldAccept` [ "foo: 1337", "bar: 0" ]

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
