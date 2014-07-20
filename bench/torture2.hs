{-
A simple torture benchmark: run some command a ridiculous
number of times against a running MPD instance.
-}

module Main (main) where

import MPD

import Control.Applicative ((<$>), (<*>))
import Control.Monad (replicateM_)

main :: IO ()
main = replicateM_ 100 $ do
  run listAllInfo >>= print

