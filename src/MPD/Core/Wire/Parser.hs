{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe              #-}

{-|
Module      : MPD.Core.Wire.Parser
Copyright   : (c) Joachim Fasting, 2015

License     : MIT
Maintainer  : joachifm@fastmail.fm
Stability   : unstable
Portability : unportable
-}

module MPD.Core.Wire.Parser where

import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as SB
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

type ProtocolVersion = (Int, Int, Int)

heloP :: A.Parser ProtocolVersion
heloP = "OK MPD " *> ((,,) <$> A.decimal <* A.char '.'
                           <*> A.decimal <* A.char '.'
                           <*> A.decimal <* A.char '\n')

protocolErrorP :: A.Parser (Int, Int, T.Text, T.Text)
protocolErrorP = (,,,) <$> -- note: expect that we've already parsed "ACK "
  (A.char '[' *> A.decimal <* A.char '@') <*>
  (A.decimal <* A.string "] {") <*>
  (T.decodeUtf8 <$> A.takeWhile1 (/= '}') <* A.string "} ") <*>
  (T.decodeUtf8 <$> A.takeWhile1 (/= '\n')) {- <* A.char '\n')) -}

responseP :: A.Parser a -> A.Parser (Either SB.ByteString a)
responseP p = A.eitherP
  ("ACK " *> A.takeWhile1 (/= '\n') <* A.char '\n')
  (p <* "list_OK\n")
