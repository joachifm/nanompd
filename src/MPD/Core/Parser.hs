{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}

{-|
Module      : MPD.Core.Parser
Copyright   : (c) Joachim Fasting, 2014

License     : MIT
Maintainer  : joachifm@fastmail.fm
Stability   : unstable
Portability : unportable
-}

module MPD.Core.Parser
  (
    Parser(..)
  , parse
  , liftP

  , boolP
  , intP
  , doubleP
  , textP

  , Label
  , field
  , field_
  ) where

import Control.Applicative
import Control.Monad (MonadPlus(..), ap)
import Control.Monad.State (State, evalState, get, put)

import Data.Monoid (Monoid(..))
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString as SB
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

------------------------------------------------------------------------
-- $parser

newtype Parser a = P { runP :: State [SB.ByteString] (Either String a) }

instance Functor Parser where
  fmap f (P g) = P (fmap (fmap f) g)

instance Monad Parser where
  return x = P $ return (Right x)
  f >>= k  = P $ either (return . Left) (runP . k) =<< runP f
  fail     = P . return . Left

instance MonadPlus Parser where
  mzero = P $ return (Left mempty)
  f `mplus` k = P $ either (\_ -> runP k) (return . Right) =<< runP f

instance Applicative Parser where
  pure  = return
  (<*>) = ap

instance Alternative Parser where
  empty = mzero
  (<|>) = mplus

parse :: Parser a -> [SB.ByteString] -> Either String a
parse = evalState . runP

-- Convert value parser into a line parser (i.e., consumes an entire line)
liftP :: A.Parser a -> Parser a
liftP p = P $ do
  st <- get
  case st of
   []   -> return (Left "liftP: empty input")
   l:ls -> case A.parseOnly p l of
     Left e  -> return (Left $ "liftP: parse value failed: " ++ e)
     Right x -> put ls >> return (Right x)

------------------------------------------------------------------------
-- $scalar

boolP :: A.Parser Bool
boolP = pure True <* A.char '1' <|> pure False <* A.char '0'

doubleP :: A.Parser Double
doubleP = A.double

intP :: A.Parser Int
intP = A.decimal

textP :: A.Parser T.Text
textP = T.decodeUtf8 <$> A.takeByteString

------------------------------------------------------------------------
-- $object

type Label = SB.ByteString

field :: Label -> A.Parser a -> Parser (Label, a)
field k p = P $ do
  st <- get
  case st of
   l:ls -> case pairBS l of
     (k', v)
       | k' == k -> either (return . Left)
                           (\x -> put ls >> return (Right (k, x)))
                           (A.parseOnly p v)
       | otherwise -> return (Left $ "field: key mismatch: " ++ show k ++ "/" ++ show k')
   [] -> return (Left "field: empty input")
  where
    pairBS x = let (hd, tl) = SB.break (== 58) {- : -} x in (hd, SB.drop 2 tl)

{-
Note: the above is more elegantly stated as

fieldP :: Label -> A.Parser a -> A.Parser (Label, a)
fieldP k v = (,) <$> (A.string k <* A.string ": ") <*> v

which is about 2x as slow ...
-}

field_ :: Label -> A.Parser a -> Parser a
field_ k v = fmap snd (field k v)
