{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}

module MPD.Commands.ParserSpec (spec) where

import Util
import Gen

import MPD.Commands.Parser

import Control.Applicative
import qualified Data.Attoparsec.ByteString as A

import Data.Monoid (mconcat, (<>))
import qualified Data.ByteString.Char8 as SB

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

------------------------------------------------------------------------
-- Main specification.

spec :: Spec
spec = do

  prop "dateP" $ forAll dateG $ \x ->
    dateP `shouldAccept` x

  let volumeG = suchThat arbitrary (\x -> x >= 0 && x <= 100)
  prop "volumeP" $ forAll volumeG $ \x ->
    A.parseOnly volumeP (fromShow x) `shouldBe` Right (Just x)

  prop "pathP" $ forAll pathG $ \x ->
    pathP `shouldAccept` x

  let positive = suchThat arbitrary (\x -> x >= 0)
      timeG = (,) <$> positive <*> positive
  prop "timeElapsedP" $ forAll timeG $ \(x, y) ->
    A.parseOnly timeElapsedP (fromShow x <> ":" <> fromShow y)
    `shouldBe` Right (x, y)

  let audioG = (,,) <$> positive <*> positive <*> positive
  prop "audioP" $ forAll audioG $ \(x, y, z) ->
    A.parseOnly audioP (mconcat [ fromShow x, ":"
                                , fromShow y, ":"
                                , fromShow z ])
    `shouldBe` Right (x, y, z)

  prop "songInfo/singleton" $ forAll songInfoG $ \x ->
    songInfoP `shouldAccept` SB.unlines x

  prop "songInfo/many" $ forAll (listOf1 songInfoG) $ \x ->
    A.many1 songInfoP `shouldAccept` SB.unlines (concat x)
