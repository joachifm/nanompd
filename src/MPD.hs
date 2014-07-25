{-|
Module      : MPD
Copyright   : (c) Joachim Fasting
License     : MIT

Maintainer  : joachifm@fastmail.fm
Stability   : unstable
Portability : unportable

This module defines the top-level client API, intended for
consumption by end users and client implementors.
-}

module MPD
  (
    -- * Usage
    -- $usage

    module MPD.Commands
  , module MPD.Core
  , module MPD.Types
  ) where

import MPD.Commands
import MPD.Core (Command, run, runWith)
import MPD.Types
  (
    Label
  , Path
  , Range
  , Seconds
  , SongPos
  , SongId
  , SubsystemName
  , Volume

  , LsEntry(..)
  , LsEntryInfo(..)
  , SongInfo(..)
  , viewTag
  , StatusInfo(..)
  )

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
  MPD.run (MPD.add ["music/foo"] *> MPD.play Nothing)
  (cur, sta) \<- MPD.run ((,) \<$\> MPD.currentSong \<*\> MPD.status)
  putStr . unlines $ map show [ cur, sta ]
@
-}
