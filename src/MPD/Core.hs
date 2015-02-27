{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}

{-|
Module      : MPD.Core
Description : Core definitions
Copyright   : (c) Joachim Fasting, 2015

License     : MIT
Maintainer  : joachifm@fastmail.fm
Stability   : unstable
Portability : unportable

This module defines types and functions for scripting client interactions
with MPD, mainly useful for users wishing to extend the command set.
-}

module MPD.Core (
  -- * Parsers
  boolP, intP, textP, floatP, pairP, fieldP,

  -- * Command specification
  Command, command,
  module MPD.Core.CommandStr,

  -- * Running commands
  ClientError(..), ProtocolVersion, run, withConn, simple,
  ) where

import MPD.Core.CommandStr
import MPD.Core.ClientError

import Control.Applicative
import Control.Monad
import Data.Functor
import Data.Monoid

import Control.Exception (bracket)
import Control.Monad.Trans.Except (ExceptT(..))

import System.IO.Error

import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8            as SB
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as T

import Network (HostName, PortID(..), connectTo)
import System.IO (Handle, hClose)

------------------------------------------------------------------------

boolP :: A.Parser Bool
boolP = A.char '0' $> False <|> A.char '1' $> True

intP :: A.Parser Int
intP = A.decimal

floatP :: A.Parser Double
floatP = A.double

textP :: A.Parser T.Text
textP = T.decodeUtf8 <$> A.takeWhile1 (/= '\n')

pairP :: SB.ByteString -> A.Parser a -> A.Parser (SB.ByteString, a)
pairP k v = (,) <$> A.string k <* A.string ": " <*> v <* A.char '\n'

fieldP :: SB.ByteString -> A.Parser a -> A.Parser a
fieldP k v = snd <$> pairP k v

------------------------------------------------------------------------

type ProtocolVersion = (Int, Int, Int)

helo :: A.Parser ProtocolVersion
helo = "OK MPD " *> ((,,) <$> A.decimal <* A.char '.'
                          <*> A.decimal <* A.char '.'
                          <*> A.decimal <* A.char '\n')

pack :: [T.Text] -> SB.ByteString
pack = T.encodeUtf8 . T.unlines
     . ("command_list_ok_begin" :)
     . (++ ["command_list_end"])
     . filter (not . T.null)

protocolError :: A.Parser (Int, Int, T.Text, T.Text)
protocolError = ((,,,) <$> -- note: expect that we've already parsed "ACK "
  (A.char '[' *> A.decimal <* A.char '@') <*>
  (A.decimal <* A.string "] {") <*>
  (T.decodeUtf8 <$> A.takeWhile1 (/= '}') <* A.string "} ") <*>
  (T.decodeUtf8 <$> A.takeWhile1 (/= '\n'))) {- <* A.char '\n')) -}

responseP :: A.Parser a -> A.Parser (Either SB.ByteString a)
responseP p = A.eitherP
  ("ACK " *> A.takeWhile1 (/= '\n') <* A.char '\n')
  (p <* "list_OK\n")

------------------------------------------------------------------------

-- Note: wrapping the parser in ExceptT is intended to allow
-- stopping the parsing process when we encounter an ACK.
-- The left value is the protocol error message from MPD, which we defer
-- parsing further until the command is run.

data Command a = Command [CommandStr] (ExceptT SB.ByteString A.Parser a)
  deriving (Functor)

instance Applicative Command where
  pure = Command [] . pure
  Command q1 p1 <*> Command q2 p2 = Command (q1 ++ q2) (p1 <*> p2)

instance (Monoid a) => Monoid (Command a) where
  mempty  = pure mempty
  mappend = liftA2 mappend

command :: CommandStr -> A.Parser a -> Command a
command q = Command [q] . ExceptT . responseP

------------------------------------------------------------------------

io :: IO a -> ExceptT ClientError IO a
io m = ExceptT $ either (Left . ConnError) Right <$> tryIOError m

send :: Handle -> [CommandStr] -> ExceptT ClientError IO ()
send hdl = io . SB.hPut hdl . pack . map render

recv :: Handle -> A.Parser a -> ExceptT ClientError IO (A.Result a)
recv hdl p = io $ A.parseWith (SB.hGetSome hdl kBUFSIZ) p ""

-- Feed input n octets at a time (TODO: what is a good size here?)
kBUFSIZ = 1024 :: Int

run :: Handle -> Command a -> ExceptT ClientError IO a
run hdl (Command q p) = do
  send hdl q
  -- Note: The ugliness of this code indicates that this design is bad,
  -- but it kind of works for now.
  res <- recv hdl (runExceptT p)
  ExceptT $ return $
    case res of

      A.Done l r -> case r of

        Left e  -> case A.parseOnly protocolError e of
          Left e'            -> Left (Custom e')
          Right (a, b, c, d) -> Left (ProtocolError a b c d)

        Right x
          | l == "OK\n" -> Right x
          | otherwise   -> Left (ParseError $ "left-overs: " ++ show l)

      A.Fail i _ e -> Left (ParseError $ "failed parsing " ++ show i ++ "; " ++ e)
      A.Partial _  -> Left (ParseError $ "insufficient input")

withConn :: HostName -> PortID -> (Handle -> IO a) -> IO a
withConn host port = bracket
  (do hdl <- connectTo host port
      _   <- A.parseOnly helo <$> SB.hGetLine hdl
      return hdl)
  (\hdl -> SB.hPut hdl "close\n" >> hClose hdl)

simple :: Command a -> IO (Either ClientError a)
simple cmd = withConn host port $ \hdl -> runExceptT (run hdl cmd)
  where (host, port) = ("localhost", PortNumber 6600)
