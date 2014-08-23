{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}

{-|
Module      : MPD.Core.CommandStr
Copyright   : (c) Joachim Fasting, 2014

License     : MIT
Maintainer  : joachifm@fastmail.fm
Stability   : unstable
Portability : unportable
-}

module MPD.Core.CommandStr ( CommandStr, (.+), render ) where

import MPD.Core.CommandArg

import Data.Monoid (Monoid(..))
import Data.String (IsString(..))
import qualified Data.Text as T

data CommandStr = CommandStr [T.Text]

instance Monoid CommandStr where
  mempty = CommandStr []
  CommandStr a `mappend` CommandStr b = CommandStr (a `mappend` b)

instance IsString CommandStr where
  fromString x = CommandStr [T.pack x]

(.+) :: (CommandArg a) => CommandStr -> a -> CommandStr
CommandStr s .+ a = CommandStr (s ++ [fromArg a])

render :: CommandStr -> T.Text
render (CommandStr as) = T.unwords (filter (not . T.null) as)
