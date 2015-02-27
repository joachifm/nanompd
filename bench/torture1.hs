{-
A simple torture benchmark: run some command a ridiculous
number of times against a running MPD instance.
-}

module Main (main) where

import MPD

import Control.Applicative
import Control.Monad (replicateM_, void)
import Control.Monad.Trans.Except (runExceptT)

main :: IO ()
main = void $ replicateM_ 100000 $ do
  simple ((,) <$> status <*> currentSong)
