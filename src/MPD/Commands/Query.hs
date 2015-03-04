{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}

{-|
Module      : MPD.Commands.Query
Copyright   : (c) Joachim Fasting, 2014

License     : MIT
Maintainer  : joachifm@fastmail.fm
Stability   : unstable
Portability : unportable

Convenient syntax for database queries.
-}

module MPD.Commands.Query (
    Query
  , (=?)
#ifdef TEST
  , queryTerms
#endif
  ) where

import MPD.Commands.Types
import MPD.Core.CommandArg (CommandArg(..))

import qualified Data.Text as T

{-|
A database query consisting of 0 or more terms.

@
Title =? "FooBar" <> Artist =? "BarFoo"
@

matches items with title "FooBar" and artist "BarFoo".

Use 'mempty' to create a query which matches anything.
-}
newtype Query = Query { queryTerms :: [(Metadata, Text)] }

instance Monoid Query where
  mempty = Query []
  Query a `mappend` Query b = Query (a `mappend` b)

instance CommandArg Query where
  fromArg = T.unwords . map f . queryTerms
    where f (m, s) = T.unwords [fromArg m, fromString (show s)]

(=?) :: Metadata -> Text -> Query
m =? s = Query [(m, s)]
