{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import MPD.Util
import Criterion.Main (defaultMain, bench, nf)

main = defaultMain [
    bench "pair"       $ nf pair "key: value"
  , bench "cyclesWith" $ nf (cyclesWith (== 0)) ([0, 1, 0, 2]::[Int])
  , bench "cycles"     $ nf (cycles [0, 1]) ([(0, 20), (1, 50)]::[(Int, Int)])
  ]
