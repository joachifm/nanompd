{-|
Module      : MPD.CommandStr
Copyright   : (c) Joachim Fasting
License     : MIT

Stability   : unstable
Portability : portable

A convenient syntax for describing MPD protocol commands.
-}

module MPD.CommandStr
  (
    -- * Usage
    -- $usage

    -- * Command strings
    CommandStr

    -- * Building command strings
  , (.+)

    -- * Rendering command strings
  , render
  ) where

import MPD.Lit

import Data.Monoid (Monoid(..), (<>))
import Data.String (IsString(..))
import qualified Data.Text      as T

{-$usage

The syntax permits incremental specification of command
strings by specifying the protocol command name and appending
any argument literals to it.

To implement support for a new argument literal, add
instances to 'FromLit' and also to 'ToLit' if the
dual is desired.

With @-XOverloadedStrings@ enabled, use thus

>>> render "status" == "status"
>>> render ("repeat" .+ False) == "repeat 0"
>>> render ("play" .+ Nothing) == "play"
>>> render ("play" .+ Just 10) == "play 10"
>>> render ("foo" .+ 10 .+ True) == "foo 10 1"
-}

{-|
An opaque representation of a command string.
-}
data CommandStr = CommandStr T.Text [T.Text]
  deriving (Show)

{-|
With @-XOverloadedStrings@, string literals are interpreted
as 'CommandStr' with no arguments.
-}
instance IsString CommandStr where
  fromString x = CommandStr (T.pack x) []
  {-# INLINE fromString #-}

{-|
@
CommandStr name xs <> CommandStr _ ys = CommandStr name (xs <> ys)
@
-}
instance Monoid CommandStr where
  CommandStr n xs `mappend` CommandStr _ ys = CommandStr n (xs ++ ys)
  {-# INLINE mappend #-}

  mempty = CommandStr mempty []
  {-# INLINE mempty #-}

{-|
Append an argument literal to a 'CommandStr'.
-}
(.+) :: (FromLit a) => CommandStr -> a -> CommandStr
CommandStr name args .+ arg = CommandStr name (args ++ [fromLit arg])
{-# INLINE (.+) #-}

{-|
Render the textual representation of a 'CommandStr'.
-}
render :: CommandStr -> T.Text
render (CommandStr name args) = T.unwords (name : filter (not . T.null) args)
{-# INLINE render #-}
