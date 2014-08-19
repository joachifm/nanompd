{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}

module MPD.Core.ParserSpec (spec) where

import MPD.Core
import Test.Hspec
import Test.Hspec.Expectations.Contrib

spec :: Spec
spec = do
  it "field/single" $ do
    field "foo" intP `shouldAccept` [ "foo: 1337" ]

  it "field/compound" $ do
    ((,) <$> field "foo" intP <*> field "bar" boolP)
    `shouldAccept` [ "foo: 1337", "bar: 0" ]
    
------------------------------------------------------------------------
-- Internal helpers.

p `shouldAccept` i = parse p i `shouldSatisfy` isRight
