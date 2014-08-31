{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}

module MPD.Core.ParserSpec (spec) where

import MPD.Core

import Control.Applicative
import qualified Data.ByteString.Char8 as SB8

import Test.Hspec
import Test.Hspec.Expectations.Contrib
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

spec :: Spec
spec = do

  prop "boolP" $ forAll (elements ([0, 1]::[Int])) $ \x ->
    liftP boolP `shouldAccept` [ SB8.pack (show x) ]

  prop "doubleP" $ forAll (arbitrary :: Gen Double) $ \x ->
    liftP doubleP `shouldAccept` [ SB8.pack (show x) ]

  prop "intP" $ forAll (suchThat arbitrary (>= 0)) $ \x -> do
    liftP intP `shouldAccept` [ SB8.pack (show (x::Int)) ]

  prop "textP" $ forAll (SB8.pack <$> listOf arbitrary) $ \x -> do
    liftP textP `shouldAccept` [ x ]

  it "field/single" $ do
    field "foo" intP `shouldAccept` [ "foo: 1337" ]

  it "field/compound" $ do
    ((,) <$> field "foo" intP <*> field "bar" intP)
    `shouldAccept` [ "foo: 1337", "bar: 7331" ]
    
------------------------------------------------------------------------
-- Internal helpers.

p `shouldAccept` i = parse p i `shouldSatisfy` isRight
