{-# LANGUAGE OverloadedStrings #-}

module MPD.Core.CommandStrSpec (spec) where

import MPD.Core
import Test.Hspec

spec :: Spec
spec = do
  it "singleton" $ do
    render "foo_cmd" == "foo_cmd"

  it "scalar" $ do
    render ("foo_cmd" .+ (10 :: Int)) == "foo_cmd 10"

  it "optional/Just" $ do
    render ("foo_cmd" .+ (Just 10 :: Maybe Int)) == "foo_cmd 10"

  it "optional/Nothing" $ do
    render ("foo_cmd" .+ (Nothing :: Maybe Int)) == "foo_cmd"

  it "choice/Left" $ do
    render ("foo_cmd" .+ (Left "foo" :: Either Text Int)) == "foo_cmd foo"

  it "choice/Right" $ do
    render ("foo_cmd" .+ (Right 10 :: Either Text Int)) == "foo_cmd 10"
