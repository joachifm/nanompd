{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}

{-|
Module      : MPD.Core.Command
Copyright   : (c) Joachim Fasting, 2014

License     : MIT
Maintainer  : joachifm@fastmail.fm
Stability   : unstable
Portability : unportable
-}

module MPD.Core.Command (
    Command(commandReq, commandRes)
  , command
  ) where

import MPD.Core.CommandStr
import MPD.Core.Parser

import Control.Arrow (second)
import Control.Applicative
import Control.Monad.State (get, put)
import qualified Data.List as List

data Command a = Command
  { commandReq :: [CommandStr]
  , commandRes :: Parser a
  }

instance Functor Command where
  fmap f (Command q p) = Command q (fmap f p)

instance Applicative Command where
  pure x = Command [] (pure x)
  Command q1 p1 <*> Command q2 p2 = Command (q1 ++ q2) (p1 <*> p2)

command :: CommandStr -> Parser a -> Command a
command q p = Command [q] $ P $ do
  (hd, tl) <- second (List.drop 1) . List.break (== "list_OK") <$> get
  rv <- put hd >> runP p
  put tl
  return rv
