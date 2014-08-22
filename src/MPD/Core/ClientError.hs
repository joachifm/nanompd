{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE Safe #-}

{-|
Module      : MPD.Core.ClientError
Copyright   : (c) Joachim Fasting, 2014

License     : MIT
Maintainer  : joachifm@fastmail.fm
Stability   : unstable
Portability : unportable
-}

module MPD.Core.ClientError (ClientError(..)) where

import Data.Text (Text)

data ClientError
  = ParseError String
  | ProtocolError
    { errorCode :: Int
    , errorListNum :: Int
    , errorCommandName :: Text
    , errorMessage :: Text
    }
  | InvalidHost
  | ConnError IOError
  | Custom String
    deriving (Show)
