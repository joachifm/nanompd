{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}

{-|
Module      : MPD.Core.Parser
Copyright   : (c) Joachim Fasting, 2015

License     : MIT
Maintainer  : joachifm@fastmail.fm
Stability   : unstable
Portability : unportable
-}

module MPD.Core.Parser where

import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.Text.Encoding               as T

boolP :: A.Parser Bool
boolP = A.char '0' $> False <|> A.char '1' $> True

intP :: A.Parser Int
intP = A.decimal

floatP :: A.Parser Double
floatP = A.double

textP :: A.Parser Text
textP = T.decodeUtf8 <$> A.takeWhile1 (/= '\n')

pairP :: ByteString -> A.Parser a -> A.Parser (ByteString, a)
pairP k v = (,) <$> A.string k <* A.string ": " <*> v <* A.char '\n'

fieldP :: ByteString -> A.Parser a -> A.Parser a
fieldP k v = snd <$> pairP k v
