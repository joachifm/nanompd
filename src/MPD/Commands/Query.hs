{-# OPTIONS_HADDOCK show-extensions #-}
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

module MPD.Commands.Query ( Query, (=?), (<&>), anything ) where

import MPD.Commands.Types
import MPD.Core (CommandArg(..))
import Data.Monoid (Monoid(..))
import qualified Data.Text as T

{-|
A database query consisting of 0 or more terms.

@
Title =? "FooBar" <&> Artist =? "BarFoo"
@

would match items with title "FooBar" and artist "BarFoo".
-}
newtype Query = Query { queryTerms :: [(Metadata, T.Text)] }
  deriving (Show)

instance Monoid Query where
  mempty = Query []
  Query a `mappend` Query b = Query (a `mappend` b)

instance CommandArg Query where
  fromArg = T.unwords . map f . queryTerms
    where f (m, s) = T.unwords [fromArg m, T.pack (show s)]

(=?) :: Metadata -> T.Text -> Query
m =? s = Query [(m, s)]

infixr 6 <&>
(<&>) :: Query -> Query -> Query
(<&>) = mappend

-- | The empty query, matches anything.
anything :: Query
anything = mempty
