{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}

{-|
Module      : MPD.Core.CommandArg
Copyright   : (c) Joachim Fasting, 2014

License     : MIT
Maintainer  : joachifm@fastmail.fm
Stability   : unstable
Portability : unportable
-}

module MPD.Core.CommandArg ( CommandArg(..) ) where

import Data.Bool (bool)
import qualified Data.Text as T

class CommandArg a where
  fromArg :: a -> T.Text

instance (CommandArg a) => CommandArg (Maybe a) where
  fromArg = maybe mempty fromArg

instance (CommandArg a, CommandArg b) => CommandArg (Either a b) where
  fromArg = either fromArg fromArg

instance (CommandArg a) => CommandArg [a] where
  fromArg = T.unwords . map fromArg

instance CommandArg Int where
  fromArg = T.pack . show

instance CommandArg Integer where
  fromArg = T.pack . show

instance CommandArg Double where
  fromArg = T.pack . show

instance CommandArg Bool where
  fromArg = bool "0" "1"

instance CommandArg T.Text where
  fromArg = id
