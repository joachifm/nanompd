{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import MPD
import MPD.Core
import MPD.Commands.Parser
import Criterion.Main
import qualified Data.Attoparsec.ByteString as A

main = defaultMain [
    bench "boolP/valid" $ whnf (A.parseOnly boolP) "0"
  , bench "boolP/invalid" $ whnf (A.parseOnly boolP) "foo"

  , bench "intP/valid" $ whnf (A.parseOnly intP) "1337"
  , bench "intP/invalid" $ whnf (A.parseOnly intP) "foo"

  , bench "doubleP/valid" $ whnf (A.parseOnly doubleP) "1337.5"
  , bench "doubleP/invalid" $ whnf (A.parseOnly doubleP) "foo"

  , bench "textP" $ whnf (A.parseOnly textP) "foo"

  , bench "pathP" $ whnf (A.parseOnly pathP) "foo/bar/baz.mp3"
  , bench "dateP" $ whnf (A.parseOnly dateP) "2014-05-16T17:33:26Z"

  , bench "field/single" $
      whnf (parse (field "key" intP)) ["key: 1337"]
  , bench "field/compound" $
      whnf (parse ((,) <$> field "a" intP <*> field "b" boolP)) ["a: 1337", "b: 0"]
  ]
