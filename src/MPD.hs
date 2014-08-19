{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE Trustworthy #-}

{-|
Module      : MPD
Description : Scripting client interactions with MPD
Copyright   : (c) Joachim Fasting, 2014

License     : MIT
Maintainer  : joachifm@fastmail.fm
Stability   : unstable
Portability : unportable

Types and functions for scripting client interactions with a running instance
of MPD, the music player daemon.
-}

module MPD
  ( module MPD.Core
  , module MPD.Commands
  ) where

import MPD.Core (
    ClientError(..)
  , Command
  , run
  , runWith
  , Label
  , Text
  )
import MPD.Commands

{-$usage
Produce a crude report of the currently playing song
and the daemon's status information:

@
import MPD

main = either (fail . show) print =<< (runEitherT . run)
  ((,) \<$\> currentSong \<*\> status)
@
-}
