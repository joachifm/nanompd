module Util where

import Test.Hspec
import Test.Hspec.Expectations.Contrib
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as SB

shouldAccept :: (Show a) => A.Parser a -> SB.ByteString -> Expectation
p `shouldAccept` i = A.parseOnly p i `shouldSatisfy` isRight
