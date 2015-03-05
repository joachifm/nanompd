{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE Safe           #-}

module InternalPrelude (
  module X,
  map,
  (++),
  length,
  ) where

import "base" Prelude as X (
  Enum(..), Bounded(..),

  Num(..), Integral(..), Fractional(..),

  Double, Int, Integer,

  ($!), seq,

  undefined,

  Show(..), Read(..), reads,
  )

import Data.Either as X (Either(..), either)
import Data.Maybe as X (Maybe(..), maybe)
import Data.Tuple as X (fst, snd)
import Data.Function as X (flip, const, ($))

import Data.Monoid as X (Monoid(..), (<>))
import Data.Functor as X (Functor(..), (<$>), ($>), void)
import Data.Foldable as X (
  Foldable, foldr, foldl',
  concat, concatMap,
  sum, product, any, all, elem, notElem,
  )
import Data.Traversable as X (
  Traversable, sequence, traverse,
  )

import Control.Applicative as X (Applicative(..), Alternative(..), liftA2, optional)
import Control.Category as X ((.), id)
import Control.Monad as X (Monad(..), MonadPlus(..), (>=>), (<=<), (=<<), join)

import Data.Eq as X (Eq(..))
import Data.Ord as X (Ord(..), Ordering(..), comparing)

import Data.Bool as X (Bool(..), bool, not, (&&), (||), otherwise)

import Data.ByteString as X (ByteString)
import Data.Text as X (Text)
import Data.String as X (String, IsString(..))

map :: (Functor f) => (a -> b) -> f a -> f b
map = {-# SCC "map" #-} X.fmap
{-# INLINE map #-}

(++) :: (MonadPlus m) => m a -> m a -> m a
(++) = {-# SCC "++" #-} mplus
{-# INLINE (++) #-}

length :: (Foldable t, Integral a) => t e -> a
length = {-# SCC "length" #-} X.foldl' (\z _ -> z + 1) 0
{-# INLINE length #-}
