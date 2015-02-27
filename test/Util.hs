module Util where

import Test.Hspec
import Data.Either (isRight)
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as SB

shouldAccept :: (Show a) => A.Parser a -> SB.ByteString -> Expectation
p `shouldAccept` i = A.parseOnly p i `shouldSatisfy` isRight
