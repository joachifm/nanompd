{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}

module MPD.Core.Wire.PacketSpec (spec) where

import MPD.Core.Wire.Packet

import Test.Hspec

spec :: Spec
spec = do

  it "pack/empty" $ do
    pack [] `shouldBe` ""

  it "pack/nullstr" $ do
    pack [""] `shouldBe` ""

  it "pack/singleton" $ do
    pack ["ping"] `shouldBe` "ping\n"

  it "pack/many" $ do
    pack ["play", "pause"] `shouldBe` "command_list_ok_begin\nplay\npause\ncommand_list_end\n"
