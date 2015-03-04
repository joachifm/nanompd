{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE Trustworthy #-}

{-|
Module      : MPD.Core.ClientError
Copyright   : (c) Joachim Fasting, 2014

License     : MIT
Maintainer  : joachifm@fastmail.fm
Stability   : unstable
Portability : unportable
-}

module MPD.Core.ClientError (ClientError(..)) where

import Network (HostName, PortID)
import System.IO.Error (IOError)

data ClientError
  = ParseError String
  | ProtocolError
    { errorCode :: Int
    , errorListNum :: Int
    , errorCommandName :: Text
    , errorMessage :: Text
    }
  | InvalidHost HostName PortID
  | ConnError IOError
  | Custom String
    deriving (Show)
