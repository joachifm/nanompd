{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}

{-|
Module      : MPD.Core.Parser
Copyright   : (c) Joachim Fasting, 2015

License     : MIT
Maintainer  : joachifm@fastmail.fm
Stability   : unstable
Portability : unportable
-}

module MPD.Core.Parser where

import Control.Applicative
import Data.Functor

import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8            as SB
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as T

boolP :: A.Parser Bool
boolP = A.char '0' $> False <|> A.char '1' $> True

intP :: A.Parser Int
intP = A.decimal

floatP :: A.Parser Double
floatP = A.double

textP :: A.Parser T.Text
textP = T.decodeUtf8 <$> A.takeWhile1 (/= '\n')

pairP :: SB.ByteString -> A.Parser a -> A.Parser (SB.ByteString, a)
pairP k v = (,) <$> A.string k <* A.string ": " <*> v <* A.char '\n'

fieldP :: SB.ByteString -> A.Parser a -> A.Parser a
fieldP k v = snd <$> pairP k v
