{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE Trustworthy #-}

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
  , simple
  ) where

import MPD.Commands
import MPD.Core (Command, ClientError(..), run, withConn)
import Prelude hiding (repeat)
import Control.Monad.Trans.Except (runExceptT)
import Network (PortID(..))

{-$usage
Produce a crude report of the currently playing song
and the daemon's status information:

@
import MPD

main = either (fail . show) print =<< 'simple'
  ((,) \<$\> 'currentSong' \<*\> 'status')
@
-}

simple :: Command a -> IO (Either ClientError a)
simple cmd = withConn host port $ \hdl -> runExceptT (run hdl cmd)
  where (host, port) = ("localhost", PortNumber 6600)
