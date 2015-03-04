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

import qualified Data.Text as T
import qualified Data.List as L

data CommandStr = CommandStr [Text]

instance Monoid CommandStr where
  mempty = CommandStr []
  CommandStr a `mappend` CommandStr b = CommandStr (a `mappend` b)

instance IsString CommandStr where
  fromString = CommandStr . (: []) . fromString

(.+) :: (CommandArg a) => CommandStr -> a -> CommandStr
CommandStr s .+ a = CommandStr (s ++ [fromArg a])

render :: CommandStr -> Text
render (CommandStr as) = T.unwords (L.filter (not . T.null) as)
