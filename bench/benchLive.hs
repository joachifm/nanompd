{-
Benchmarks against a live MPD instance.

Warning: make sure to compare runs between identical environments
(database, MPD version, etc).
-}

module Main (main) where

import MPD
import Control.Applicative
import Criterion.Main (defaultMain, bench, nfIO, whnfIO)
import Control.Monad.Trans.Either (runEitherT)
import Control.Monad.Trans.Except (runExceptT)

main = defaultMain
  [
    bench "ping"         $ whnfIO (simple ping)
  , bench "currentsong"  $ whnfIO (simple currentSong)
  , bench "lsinfo"       $ whnfIO (simple $ lsInfo Nothing)
  , bench "listallinfo"  $ whnfIO (simple $ listAllInfo Nothing)
  , bench "combined"     $ whnfIO (simple $ ((,,,) <$> ping
                                                   <*> currentSong
                                                   <*> lsInfo Nothing
                                                   <*> listAllInfo Nothing))
  ]
