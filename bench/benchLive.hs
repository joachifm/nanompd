{-
Benchmarks against a live MPD instance.

Warning: make sure to compare runs between identical environments
(database, MPD version, etc).
-}

module Main (main) where

import MPD
import Criterion.Main (defaultMain, bench, nfIO)

main = defaultMain
  [ bench "listallinfo" $ nfIO (run listallinfo)
  , bench "currentsong" $ nfIO (run currentsong)
  , bench "ping"        $ nfIO (run ping)
  ]
