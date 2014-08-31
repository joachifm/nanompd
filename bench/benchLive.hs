{-
Benchmarks against a live MPD instance.

Warning: make sure to compare runs between identical environments
(database, MPD version, etc).
-}

module Main (main) where

import MPD
import Criterion.Main (defaultMain, bench, whnfIO)
import Control.Monad.Trans.Either (runEitherT)

main = defaultMain
  [
    bench "ping"           $ whnfIO (runEitherT . run $ ping)
  , bench "currentsong"    $ whnfIO (runEitherT . run $ currentSong)
  , bench "listallinfo"    $ whnfIO (runEitherT . run $ listAllInfo Nothing)
  ]
