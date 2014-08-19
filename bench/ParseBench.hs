module Main (main) where

import MPD
import Criterion.Main

main = defaultMain [
    bench "boolP/valid" $ whnf boolP "0"
  , bench "boolP/invalid" $ whnf boolP "foo"

  , bench "intP/valid" $ whnf intP "1337"
  , bench "intP/invalid" $ whnf intP "foo"

  , bench "doubleP/valid" $ whnf doubleP "1337.5"
  , bench "doubleP/invalid" $ whnf doubleP "foo"

  , bench "textP" $ whnf textP "foo"

  , bench "labelP" $ whnf labelP "foo"
  , bench "pathP" $ whnf pathP "foo/bar/baz.mp3"
  , bench "dateP" $ whnf dateP "2014-05-16T17:33:26Z"

  , bench "field/single" $
      whnf (parse (field "key" intP)) ["key: 1337"]
  , bench "field/compound" $
      whnf (parse ((,) <$> field "a" intP <*> field "b" boolP)) ["a: 1337", "b: 0"]
  ]
