{-
Benchmarks against a live MPD instance.

Warning: make sure to compare runs between identical environments
(database, MPD version, etc).
-}

module Main (main) where

import MPD
import Criterion.Main (defaultMain, bench, nfIO)

main = defaultMain
  [ bench "listallinfo"    $ nfIO (run listAllInfo)
  , bench "currentsong"    $ nfIO (run currentSong)
  , bench "ping"           $ nfIO (run ping)
  , bench "plChangesPosId" $ nfIO (run $ plChangesPosId 1)
  , bench "playlistinfo"   $ nfIO (run playlistInfo)
  ]
