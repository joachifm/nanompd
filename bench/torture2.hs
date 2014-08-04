{-
A simple torture benchmark: run some command a ridiculous
number of times against a running MPD instance.
-}

module Main (main) where

import MPD

import Control.Applicative ((<$>), (<*>))
import Control.Monad (replicateM_, void)

main :: IO ()
main = void $ replicateM_ 100 $ do
  (runClientT . run) (listAllInfo Nothing) >>= print
