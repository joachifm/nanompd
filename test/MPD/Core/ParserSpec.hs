{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}

module MPD.Core.ParserSpec (spec) where

import Util
import Gen
import MPD.Core

import qualified Data.ByteString.Char8 as SB8

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

spec :: Spec
spec = do

  prop "boolP" $ forAll (elements ([0, 1]::[Int])) $ \x ->
    boolP `shouldAccept` SB8.pack (show x)

  prop "floatP" $ forAll (arbitrary :: Gen Double) $ \x ->
    floatP `shouldAccept` SB8.pack (show x)

  prop "intP" $ forAll (suchThat arbitrary (>= 0)) $ \x -> do
    intP `shouldAccept` SB8.pack (show (x::Int))

  prop "textP" $ forAll textG $ \x -> do
    textP `shouldAccept` x

  it "fieldP/single" $ do
    fieldP "foo" intP `shouldAccept` "foo: 1337\n"

  it "fieldP/compound" $ do
    ((,) <$> fieldP "foo" intP <*> fieldP "bar" intP)
    `shouldAccept` "foo: 1337\nbar: 7331\n"
