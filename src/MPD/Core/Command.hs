{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE Safe              #-}

{-|
Module      : MPD.Core.Command
Description : Core definitions
Copyright   : (c) Joachim Fasting, 2015

License     : MIT
Maintainer  : joachifm@fastmail.fm
Stability   : unstable
Portability : unportable
-}

module MPD.Core.Command where

import MPD.Core.CommandStr
import MPD.Core.Wire.Parser

import Control.Applicative (liftA2)
import Control.Monad.Trans.Except
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8            as SB

-- Note: wrapping the parser in ExceptT is intended to allow stopping the
-- parsing process when we encounter an ACK.  The left value is the protocol
-- error message from MPD, which we defer parsing further until the command
-- is run.

data Command a = Command [CommandStr] (ExceptT SB.ByteString A.Parser a)
  deriving (Functor)

instance Applicative Command where
  pure = Command [] . pure
  Command q1 p1 <*> Command q2 p2 = Command (q1 ++ q2) (p1 <*> p2)

instance (Monoid a) => Monoid (Command a) where
  mempty  = pure mempty
  mappend = liftA2 mappend

command :: CommandStr -> A.Parser a -> Command a
command q = Command [q] . ExceptT . responseP
