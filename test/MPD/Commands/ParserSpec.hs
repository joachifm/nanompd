{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}

module MPD.Commands.ParserSpec (spec) where

import MPD.Core
import MPD.Commands.Parser

import Control.Applicative
import qualified Data.Attoparsec.ByteString as A

import Data.Monoid (mconcat, (<>))
import Data.String (fromString)

import Test.Hspec
import Test.Hspec.Expectations.Contrib
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

------------------------------------------------------------------------
-- Main specification.

spec :: Spec
spec = do

  prop "dateP" $ forAll dateGen $ \x ->
    A.parseOnly dateP x `shouldSatisfy` isRight

  let volumeGen = suchThat arbitrary (\x -> x >= 0 && x <= 100)
  prop "volumeP" $ forAll volumeGen $ \x ->
    A.parseOnly volumeP (fromString $ show x) `shouldBe` Right (Just x)

  it "pathP" $
    A.parseOnly pathP "foo/bar.mp3"
    `shouldSatisfy` isRight

  let positive = suchThat arbitrary (\x -> x >= 0)
      timeG = (,) <$> positive <*> positive
  prop "timeElapsedP" $ forAll timeG $ \(x, y) ->
    A.parseOnly timeElapsedP (mconcat [ fromString (show x), ":"
                                      , fromString (show y) ])
    `shouldBe` Right (x, y)

  let audioG = (,,) <$> positive <*> positive <*> positive
  prop "audioP" $ forAll audioG $ \(x, y, z) ->
    A.parseOnly audioP (mconcat [ fromString (show x), ":"
                                , fromString (show y), ":"
                                , fromString (show z) ])
    `shouldBe` Right (x, y, z)

  prop "songInfo/singleton" $ forAll songInfoGen $ \x ->
    songInfo `shouldAccept` x

  prop "songInfo/many" $ forAll (listOf songInfoGen) $ \x ->
    many songInfo `shouldAccept` (mconcat x)

------------------------------------------------------------------------
-- Generate input

dateGen = do
  (year, month, day) <- (,,) <$> digits 4 <*> digits 2 <*> digits 2
  (hours, mins, secs) <- (,,) <$> digits 2 <*> digits 2 <*> digits 2
  return (mconcat [ year, "-", month, "-", day, "T"
                  , hours, ":", mins, ":", secs, "Z" ])

songInfoGen = do
  fileName <- fromString <$> arbitrary
  lastModified <- dateGen
  time <- digits 4

  playing <- arbitrary
  pos <- if playing
         then Just <$> digits 4
         else return Nothing
  id_ <- if playing
         then Just <$> digits 4
         else return Nothing

  tags <- listOf $ do
    (k, g) <- elements tag
    v <- g
    return (k <> ": " <> v)

  return $ [ "file: " <> fileName
           , "Last-Modified: " <> lastModified
           , "Time: " <> time
           ]
           ++ maybe [] (\x -> ["Pos: " <> x]) pos
           ++ maybe [] (\x -> ["Id: " <> x]) id_
           ++ tags
  where
    tag  = [ ("Artist",      fromString <$> arbitrary)
           , ("AlbumArtist", fromString <$> arbitrary)
           , ("Title",       fromString <$> arbitrary)
           , ("Album",       fromString <$> arbitrary)
           , ("Track", do
                 a <- digits 2
                 b <- digits 2
                 return (a <> "/" <> b))
           , ("Date", digits 4)
           , ("Genre", fromString <$> arbitrary)
           ]

------------------------------------------------------------------------
-- Internal helpers.

p `shouldAccept` i = parse p i `shouldSatisfy` isRight

digits n = fromString <$> vectorOf n (elements ['0'..'9'])
