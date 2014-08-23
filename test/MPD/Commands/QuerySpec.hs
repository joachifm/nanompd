{-# LANGUAGE OverloadedStrings #-}

module MPD.Commands.QuerySpec (spec) where

import MPD.Commands.Types (Metadata(..))
import MPD.Commands.Query
import Test.Hspec
import Data.Monoid

spec :: Spec
spec = do
  it "empty" $ do
    queryTerms mempty
    `shouldBe` []

  it "Artist =? \"Foo\"" $ do
    queryTerms (Artist =? "Foo")
    `shouldBe` [ (Artist, "Foo") ]

  it "Artist =? \"Foo\" <> Title =? \"Bar\"" $ do
    queryTerms (Artist =? "Foo" <> Title =? "Bar")
    `shouldBe` [ (Artist, "Foo")
               , (Title, "Bar") ]
