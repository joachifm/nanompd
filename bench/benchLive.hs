{-
Benchmarks against a live MPD instance.

Warning: make sure to compare runs between identical environments
(database, MPD version, etc).
-}

module Main (main) where

import MPD
import Criterion.Main (defaultMain, bench, nfIO)

main = defaultMain
  [
    bench "ping"           $ nfIO (run ping)
  , bench "currentsong"    $ nfIO (run currentSong)
  , bench "listallinfo"    $ nfIO (run $ listAllInfo Nothing)
  ]
