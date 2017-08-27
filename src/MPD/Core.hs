{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : MPD.Core
Description : Core definitions
Copyright   : (c) Joachim Fasting, 2015

License     : MIT
Maintainer  : joachifm@fastmail.fm
Stability   : unstable
Portability : unportable

This module defines types and functions for scripting client interactions
with MPD, mainly useful for users wishing to extend the command set.
-}

module MPD.Core (
  -- * Parsers
  boolP, intP, textP, floatP, pairP, fieldP,

  -- * Command specification
  Command, command,
  module MPD.Core.CommandArg,
  module MPD.Core.CommandStr,

  -- * Running commands
  ClientError(..), ProtocolVersion,
  run, withConn
  ) where

import MPD.Core.ClientError
import MPD.Core.Command
import MPD.Core.CommandArg
import MPD.Core.CommandStr
import MPD.Core.Parser
import MPD.Core.Run
import MPD.Core.Wire
