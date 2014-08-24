{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import MPD
import MPD.Core
import MPD.Commands.Parser
import Criterion.Main
import qualified Data.Attoparsec.ByteString as A

main = defaultMain [
    bench "boolP" $
      whnf (A.parseOnly boolP) "0"

  , bench "doubleP" $
      whnf (A.parseOnly doubleP) "1337.25"

  , bench "intP" $
      whnf (A.parseOnly intP) "1337"

  , bench "textP" $
      whnf (A.parseOnly textP) "foo"

  , bench "field/single" $
      whnf (parse (field "key" intP)) ["key: 1337"]

  , bench "field/compound" $
      whnf (parse ((,,) <$> field "a" boolP
                        <*> field "b" doubleP
                        <*> field "c" intP)) ["a: 1", "b: 2.5", "42"]

  , bench "dateP" $
      whnf (A.parseOnly dateP) "2014-05-16T17:33:26Z"
  ]
