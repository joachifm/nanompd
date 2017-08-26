{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}

module MPD.Core.Wire.ParserSpec (spec) where

import Util
import MPD.Core.Wire.Parser

import Test.Hspec

spec :: Spec
spec = do

  it "heloP" $ do
    heloP `shouldAccept` "OK MPD 0.20.1\n"

  it "protocolErrorP" $ do
    ("ACK " *> protocolErrorP)
    `shouldAccept` "ACK [1@10] {current_command} message_text\n"
