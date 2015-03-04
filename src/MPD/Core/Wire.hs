{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy       #-}

{-|
Module      : MPD.Core.Wire
Copyright   : (c) Joachim Fasting, 2015

License     : MIT
Maintainer  : joachifm@fastmail.fm
Stability   : unstable
Portability : unportable
-}

module MPD.Core.Wire where

import Control.Applicative

import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as SB
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.List as L

type ProtocolVersion = (Int, Int, Int)

helo :: A.Parser ProtocolVersion
helo = "OK MPD " *> ((,,) <$> A.decimal <* A.char '.'
                          <*> A.decimal <* A.char '.'
                          <*> A.decimal <* A.char '\n')

pack :: [T.Text] -> SB.ByteString
pack = T.encodeUtf8 . T.unlines
     . ("command_list_ok_begin" :)
     . (++ ["command_list_end"])
     . L.filter (not . T.null)

protocolError :: A.Parser (Int, Int, T.Text, T.Text)
protocolError = (,,,) <$> -- note: expect that we've already parsed "ACK "
  (A.char '[' *> A.decimal <* A.char '@') <*>
  (A.decimal <* A.string "] {") <*>
  (T.decodeUtf8 <$> A.takeWhile1 (/= '}') <* A.string "} ") <*>
  (T.decodeUtf8 <$> A.takeWhile1 (/= '\n')) {- <* A.char '\n')) -}

responseP :: A.Parser a -> A.Parser (Either SB.ByteString a)
responseP p = A.eitherP
  ("ACK " *> A.takeWhile1 (/= '\n') <* A.char '\n')
  (p <* "list_OK\n")
