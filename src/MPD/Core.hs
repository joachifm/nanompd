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
  ProtocolVersion, run, withConn, simple,
  ) where

import MPD.Core.CommandStr
import MPD.Core.ClientError

import Control.Applicative
import Control.Monad
import Data.Functor
import Data.Monoid

import Control.Exception (bracket)
import Control.Monad.Trans.Except (ExceptT(..))

import System.IO (BufferMode(..), hSetBuffering)
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

data Command a = Command [CommandStr] (ExceptT SB.ByteString A.Parser a)
  deriving (Functor)

instance Applicative Command where
  pure = Command [] . pure
  Command q1 p1 <*> Command q2 p2 = Command (q1 ++ q2) (p1 <*> p2)

instance (Monoid a) => Monoid (Command a) where
  mempty  = pure mempty
  mappend = liftA2 mappend

command :: CommandStr -> A.Parser a -> Command a
command q p = Command [q] . ExceptT $ A.eitherP
  ("ACK " *> A.takeWhile1 (/= '\n') <* A.char '\n')
  (p <* "list_OK\n")

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
protocolError = "ACK " *> ((,,,) <$>
  (A.char '[' *> A.decimal <* A.char '@') <*>
  (A.decimal <* A.string "] {") <*>
  (T.decodeUtf8 <$> A.takeWhile1 (/= '}') <* A.string "} ") <*>
  (T.decodeUtf8 <$> A.takeWhile1 (/= '\n') <* A.char '\n'))

------------------------------------------------------------------------

io :: IO a -> IO (Either ClientError a)
io m = either (Left . ConnError) Right <$> tryIOError m

send :: Handle -> [CommandStr] -> IO ()
send hdl = SB.hPut hdl . pack . map render

recv :: Handle -> A.Parser a -> IO (A.Result a)
recv hdl p = do
  hSetBuffering hdl LineBuffering
  A.parseWith (SB.hGetSome hdl kBUFSIZ) p ""
  -- TODO: we need to throw away the final OK, but if we
  -- do @(p <* "OK\n")@ we hang forever on ACK ... maybe do it in
  -- run?

kBUFSIZ = 64 :: Int

run :: Handle -> Command a -> IO a
run hdl (Command q p) = send hdl q >> (f =<< recv hdl (runExceptT p))
  where
    f x = case A.eitherResult x of
      Right r -> case r of
        Left e   -> fail (show e)
        Right r' -> return r'
      Left e -> fail (show e)

withConn :: HostName -> PortID -> (Handle -> IO a) -> IO a
withConn host port = bracket
  (do hdl <- connectTo host port
      _   <- A.parseOnly helo <$> SB.hGetLine hdl
      return hdl)
  (\hdl -> SB.hPut hdl "close\n" >> hClose hdl)

simple :: Command a -> IO a
simple cmd = withConn host port $ \hdl -> run hdl cmd
  where (host, port) = ("localhost", PortNumber 6600)
