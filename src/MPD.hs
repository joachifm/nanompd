{-|
Module      : MPD
Copyright   : (c) Joachim Fasting
License     : MIT

Maintainer  : joachifm@fastmail.fm
Stability   : experimental
Portability : portable

This module defines the top-level client API,
intended for consumption by end users and client
implementors.
-}

module MPD
  (
    -- * Usage
    -- $usage

    -- * Running MPD protocol command wrappers
    Command
  , run
  , runWith

    -- * Pre-defined MPD protocol command wrappers
  , module MPD.Commands
  ) where

import MPD.Commands
import MPD.Core

{-$usage

The module should be imported qualified to avoid
name clashes with the standard "Prelude".

A basic usage example: add a database URI to the current
playlist, begin playback, and print a crude report comprising
current song information and daemon status information:

@
{-# LANGUAGE OverloadedStrings #-}

import qualified MPD
import Control.Applicative

foo = do
  (cur, sta) \<- MPD.run (MPD.add ["music/foo"] *\> MPD.play Nothing *\> ((,) \<$\> MPD.currentSong \<*\> MPD.status))
  putStr . unlines $ map show [ cur, sta ]
@
-}
