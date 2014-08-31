{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE Trustworthy #-}

{-|
Module      : MPD.Core.Run
Copyright   : (c) Joachim Fasting, 2014

License     : MIT
Maintainer  : joachifm@fastmail.fm
Stability   : unstable
Portability : unportable
-}

module MPD.Core.Run ( runWith, run ) where

import MPD.Core.ClientError
import MPD.Core.Command
import MPD.Core.Conn

import Control.Monad.Catch (MonadMask)
import Control.Monad.Trans (MonadIO)
import Control.Monad.Trans.Either (EitherT)
import Network (HostName, PortID(..))

------------------------------------------------------------------------

runWith
  :: (MonadMask m, MonadIO m)
  => HostName
  -> PortID
  -> Command a
  -> EitherT ClientError m a
runWith host port cmd = withConn host port $ \hdl ->
  getResponse hdl (commandReq cmd) (commandRes cmd)

run
  :: (MonadMask m, MonadIO m)
  => Command a
  -> EitherT ClientError m a
run = runWith "localhost" (PortNumber 6600)
