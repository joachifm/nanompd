{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}

module MPD.Core.ParserSpec (spec) where

import MPD.Core
import Test.Hspec
import Test.Hspec.Expectations.Contrib

spec :: Spec
spec = do
  it "boolP/true" $ do
    liftP boolP `shouldAccept` [ "1" ]

  it "boolP/false" $ do
    liftP boolP `shouldAccept` [ "0" ]

  it "doubleP" $ do
    liftP doubleP `shouldAccept` [ "2.5" ]

  it "intP" $ do
    liftP intP `shouldAccept` [ "1337" ]

  it "textP" $ do
    liftP textP `shouldAccept` [ "foo" ]

  it "field/single" $ do
    field "foo" intP `shouldAccept` [ "foo: 1337" ]

  it "field/compound" $ do
    ((,) <$> field "foo" intP <*> field "bar" intP)
    `shouldAccept` [ "foo: 1337", "bar: 7331" ]
    
------------------------------------------------------------------------
-- Internal helpers.

p `shouldAccept` i = parse p i `shouldSatisfy` isRight
