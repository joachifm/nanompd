{-# LANGUAGE OverloadedStrings #-}

module MPD.CoreSpec (spec) where

import MPD.Core

import Control.Applicative
import Control.Monad.State.Strict (runState)
import Data.ByteString (ByteString)
import Test.Hspec

spec :: Spec
spec = do
  describe "pack" $ do
    it "prepares a request body for transfer over the wire" $
      pack ["add", "play", "status"]
        `shouldBe` "command_list_ok_begin\nadd\nplay\nstatus\ncommand_list_end\n"

  describe "response" $ do
    it "reads the response from MPD as a lazy bytestring" $
      response "foo: bar\nlist_OK\nbar: foo\nlist_OK\nOK\n"
        `shouldBe` Right ["foo: bar", "list_OK", "bar: foo", "list_OK"]
    it "returns Left on protocol error" $
      response "foo: bar\nlist_OK\nACK BOOO\n" `shouldBe` Left "BOOO"
    it "returns Left on premature end-of-input" $
      response "foo: bar\n" `shouldBe` Left "premature end-of-input"

  describe "command" $ do
    it "applies a fold to all input up to the next response in the list" $ do
      let cmd = command "currentsong" id
          rsp = ["foo: bar", "list_OK", "OK"]
      let (a, _) = runState (commandRes cmd) rsp
      a `shouldBe` ["foo: bar"]

    it "can be combined with applicative" $ do
      let cmd1 = command "currentsong" id
          cmd2 = command "status" id
          rsp  = ["foo: bar", "list_OK", "zoo: boo", "list_OK", "OK"]
      let (a, _) = runState ((,) <$> commandRes cmd1 <*> commandRes cmd2) rsp
      a `shouldBe` (["foo: bar"], ["zoo: boo"])
