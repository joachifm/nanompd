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

    -- * Argument literals
  , FromLit(..), ToLit(..), Lit

    -- * Building command strings
  , (.+)

    -- * Rendering command strings
  , render
  ) where

import MPD.Util (parseDecimal, unparseDecimal)

import Data.Monoid (Monoid(..), (<>))
import Data.String (IsString(..))
import qualified Data.Text      as T
import qualified Data.Text.Read as T

{-$usage

>>> render ("repeat" .+ False) == "repeat 0"
>>> render ("play" .+ Nothing) == "play"
>>> render ("play" .+ Just 10) == "play 10"
>>> render ("foo" .+ 10 .+ True) == "foo 10 1"
-}

------------------------------------------------------------------------

{-|
A class of Haskell types which can be used as command argument literals.
-}
class FromLit a where
  fromLit :: a -> T.Text

{-|
The dual of 'FromLit'.
-}
class ToLit a where
  toLit :: T.Text -> Maybe a

{-|
A class of types which are both 'FromLit' and 'ToLit', subject to

@
forAll $ \\x -> toLit (fromLit x) == Just x
@
-}
class (FromLit a, ToLit a) => Lit a

------------------------------------------------------------------------

instance (FromLit a) => FromLit (Maybe a) where
  fromLit = maybe T.empty fromLit

instance (FromLit a) => FromLit [a] where
  fromLit = T.unwords . map fromLit

instance FromLit Integer where
  fromLit = unparseDecimal

instance ToLit Integer where
  toLit = parseDecimal

instance FromLit Int where
  fromLit = unparseDecimal

instance ToLit Int where
  toLit = parseDecimal

instance FromLit Bool where
  fromLit x = T.pack (if x then "1" else "0")

instance ToLit Bool where
  toLit = Just . (/= T.pack "0")

instance FromLit T.Text where
  fromLit x = '"' `T.cons` x `T.snoc` '"'

instance ToLit T.Text where
  toLit = Just

------------------------------------------------------------------------

{-|
A command string: the command name and a list of formatted parameter
values.
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
Note that

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
