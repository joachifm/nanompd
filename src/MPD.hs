{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE Safe #-}

{-|
Module      : MPD
Copyright   : (c) Joachim Fasting, 2014

License     : MIT
Maintainer  : joachifm@fastmail.fm
Stability   : unstable
Portability : unportable

This module exposes functionality most users will want.
See "MPD.Core" to build your own command wrappers, possibly using bits
from "MPD.Commands.Types" and "MPD.Commands.Parser".
-}

module MPD
  (
    -- * Usage
    -- $usage
    module MPD.Commands
  , module MPD.Core
  ) where

import MPD.Commands
import MPD.Core (
    ClientError(..)
  , Command
  , Label
  , run
  , runWith
  )
import Prelude hiding (repeat)

{-$usage
Produce a crude report of the currently playing song
and the daemon's status information:

@
import MPD

main = either (fail . show) print =<< ('runEitherT' . 'run')
  ((,) \<$\> 'currentSong' \<*\> 'status')
@
-}
