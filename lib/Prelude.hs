{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}

{-|
Internal Prelude: re-exports the standard Prelude along with
common modules used throughout the codebase.
-}

module Prelude (
  module BasePrelude,
  module Data.Text,
  module Data.ByteString,
  module Data.String,
  ) where

import "base" Prelude as BasePrelude

import Data.Monoid

import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.String (IsString(..))
