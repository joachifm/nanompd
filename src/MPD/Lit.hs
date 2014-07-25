{-# LANGUAGE Safe #-}

{-|
Module      : MPD.Lit
Copyright   : (c) Joachim Fasting
License     : MIT

Stability   : unstable
Portability : unportable

MPD protocol literals.
-}

module MPD.Lit ( Lit, FromLit(..), ToLit(..) ) where

import MPD.Util (parseDecimal, unparseDecimal)
import qualified Data.Text as T

------------------------------------------------------------------------
-- Classes

{-|
A class of Haskell types which can be used as command argument literals.
-}
class FromLit a where
  {-|
  Format a Haskell value as it would be transmitted by MPD.
  -}
  fromLit :: a -> T.Text

{-|
The dual of 'FromLit'.
-}
class ToLit a where
  {-|
  Parse a textual literal as it would be transmitted by MPD into
  a Haskell value.
  -}
  toLit :: T.Text -> Maybe a

{-|
A class of types which are both 'FromLit' and 'ToLit', subject to

@
forAll $ \\x -> toLit (fromLit x) == Just x
@
-}
class (FromLit a, ToLit a) => Lit a

------------------------------------------------------------------------
-- Instances for standard types.

instance (FromLit a) => FromLit (Maybe a) where
  fromLit = maybe T.empty fromLit
  {-# INLINE fromLit #-}

instance (FromLit a) => FromLit [a] where
  fromLit = T.unwords . map fromLit
  {-# INLINE fromLit #-}

instance FromLit Integer where
  fromLit = unparseDecimal
  {-# INLINE fromLit #-}

instance ToLit Integer where
  toLit = parseDecimal
  {-# INLINE toLit #-}

instance Lit Integer

instance FromLit Int where
  fromLit = unparseDecimal
  {-# INLINE fromLit #-}

instance ToLit Int where
  toLit = parseDecimal
  {-# INLINE toLit #-}

instance Lit Int

instance FromLit Bool where
  fromLit x = T.pack (if x then "1" else "0")
  {-# INLINE fromLit #-}

instance ToLit Bool where
  toLit = Just . (/= T.pack "0")
  {-# INLINE toLit #-}

instance Lit Bool

instance FromLit T.Text where
  fromLit x = '"' `T.cons` x `T.snoc` '"'
  {-# INLINE fromLit #-}

instance ToLit T.Text where
  toLit = Just
  {-# INLINE toLit #-}

instance Lit T.Text
