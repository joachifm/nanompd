{-|
Module      : MPD.Util
Copyright   : (c) Joachim Fasting
License     : MIT

Stability   : unstable
Portability : unportable

Grab-bag of utilities, some more general than others.
-}

module MPD.Util
  ( parseDecimal, unparseDecimal
  , pair
  , cycles, cyclesWith
  ) where

import Control.Arrow (second)
import qualified Data.ByteString    as SB
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read     as T

{-|
Parse a decimal value, as it would be transmitted
by MPD.
-}
parseDecimal :: (Integral a) => T.Text -> Maybe a
parseDecimal = either (const Nothing) (Just . fst) . T.decimal
{-# INLINE parseDecimal #-}

{-|
Unparse a decimal value, as it would be transmitted
by MPD.
-}
unparseDecimal :: (Integral a, Show a) => a -> T.Text
unparseDecimal = T.pack . show
{-# INLINE unparseDecimal #-}

{-|
Break a UTF-8 encoded 'SB.ByteString' into a key
and a 'T.Text' value.

The key is assumed to be limited to ASCII chars
and so is not decoded.

>>> pair "foo: bar" == ("foo", "bar")
-}
pair :: SB.ByteString -> (SB.ByteString, T.Text)
pair = second (T.decodeUtf8 . SB.drop 2) . SB.break (== 58)

{-|
A specialised version of 'cyclesWith' for grouping
association lists, using a given list of keys to
identify cycles.

>>> cycles ["file", "outputid"]
           [ ("file", "foo.mp3")
           , ("Artist", "Foo")
           , ("outputid", "0")
           , ("outputenabled", "1")
           ] == [ [("file", "foo.mp3"), ("Artist", "Foo")]
                , [("outputid", "0"), ("outputenabled", "1")] ]
-}
cycles :: (Eq k) => [k] -> [(k, e)] -> [[(k, e)]]
cycles heads = cyclesWith ((`elem` heads) . fst)

{-|
Group elements into cycles, given a predicate identifying
the beginning of a cycle.

>>> cyclesWith (== 0) []           == []
>>> cyclesWith (== 0) [0]          == [[0]]
>>> cyclesWith (== 0) [0, 1, 2]    == [[0, 1, 2]]
>>> cyclesWith (== 0) [0, 1, 0, 2] == [[0, 1], [0, 2]]
-}
cyclesWith :: (a -> Bool) -> [a] -> [[a]]
cyclesWith p = go
  where
    go []     = []
    go (x:xs) = (\(hd, tl) -> (x : hd) : go tl) (break p xs)
