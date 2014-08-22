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

module MPD.Core.CommandStr (CommandStr, CommandArg(..), (.+), render) where

import Data.Monoid (Monoid(..))
import Data.String (IsString(..))
import qualified Data.Text as T

data CommandStr = CommandStr [T.Text]
  deriving (Show)

instance Monoid CommandStr where
  mempty = CommandStr []
  CommandStr a `mappend` CommandStr b = CommandStr (a `mappend` b)

instance IsString CommandStr where
  fromString x = CommandStr [T.pack x]

class CommandArg a where
  fromArg :: a -> T.Text

instance (CommandArg a) => CommandArg (Maybe a) where
  fromArg = maybe mempty fromArg

instance (CommandArg a, CommandArg b) => CommandArg (Either a b) where
  fromArg = either fromArg fromArg

instance (CommandArg a) => CommandArg [a] where
  fromArg m = T.unwords (map fromArg m)

instance CommandArg Int where
  fromArg = T.pack . show

instance CommandArg Integer where
  fromArg = T.pack . show

instance CommandArg Double where
  fromArg = T.pack . show

instance CommandArg Bool where
  fromArg x = if x then "1" else "0"

instance CommandArg T.Text where
  fromArg = id

(.+) :: (CommandArg a) => CommandStr -> a -> CommandStr
CommandStr s .+ a = CommandStr (s ++ [fromArg a])

render :: CommandStr -> T.Text
render (CommandStr as) = T.unwords (filter (not . T.null) as)
