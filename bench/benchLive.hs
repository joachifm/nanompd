{-
Benchmarks against a live MPD instance.

Warning: make sure to compare runs between identical environments
(database, MPD version, etc).
-}

module Main (main) where

import MPD
import Criterion.Main (defaultMain, bench, whnfIO)

main = defaultMain
  [
    bench "ping"           $ whnfIO (runClientT . run $ ping)
  , bench "currentsong"    $ whnfIO (runClientT . run $ currentSong)
  , bench "listallinfo"    $ whnfIO (runClientT . run $ listAllInfo Nothing)
  ]
