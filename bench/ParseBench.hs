{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import MPD
import MPD.Core
import MPD.Commands.Parser

import Control.Applicative
import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString as SB

import Criterion.Main

main = defaultMain [
    bench "boolP" $
      nf (A.parseOnly boolP) "0"

  , bench "floatP" $
      nf (A.parseOnly floatP) "1337.25"

  , bench "intP" $
      nf (A.parseOnly intP) "1337"

  , bench "textP" $
      nf (A.parseOnly textP) "foo"

  , bench "dateP" $
      nf (A.parseOnly dateP) "2014-05-16T17:33:26Z"

  , bench "field/single" $
      nf (A.parseOnly (fieldP "key" intP)) "key: 1337\n"

  , bench "field/compound" $
      nf (A.parseOnly ((,,) <$> fieldP "a" boolP
                            <*> fieldP "b" floatP
                            <*> fieldP "c" intP)) "a: 1\nb: 2.5\nc: 42\n"

  , bench "field/complex" $ do
      let p = (,,) <$> fieldP "a" boolP <*> fieldP "b" floatP <*> fieldP "c" intP
          s = SB.concat (replicate 1000 "a: 1\nb: 2.5\nc: 42\n")
      nf (A.parseOnly $ A.many1 p) s
  ]
