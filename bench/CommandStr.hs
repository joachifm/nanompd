{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import MPD.CommandStr
import MPD.Lit

import Criterion.Main
import Data.String
import Data.Monoid
import qualified Data.Text as T

main = defaultMain [
    bench "fromString" $ nf (fromString :: String -> CommandStr) "foo"
  , bench "mappend/0" $ nf (("foo"::CommandStr) <>) "bar"
  , bench "mappend/1" $ nf (("foo"::CommandStr) .+ True <>) ("bar" .+ False)
  , bench ".+" $ nf ("foo" .+) (1::Int)
  , bench "render" $ nf render ("foo" .+ (1::Int))
  , bench "fromLit/Bool" $ nf fromLit True
  , bench "fromLit/Int"  $ nf fromLit (0::Int)
  , bench "fromLit/Text" $ nf fromLit ("FOO"::T.Text)
  ]