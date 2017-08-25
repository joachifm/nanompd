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
  module MPD.Core.CommandArg,
  module MPD.Core.CommandStr,

  -- * Running commands
  ClientError(..), ProtocolVersion,
  run, withConn
  ) where

import MPD.Core.CommandArg
import MPD.Core.CommandStr
import MPD.Core.ClientError
import MPD.Core.Parser
import MPD.Core.Wire

import Control.Applicative

import Control.Exception (bracket)
import Control.Monad.Trans.Except

import System.IO.Error

import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8            as SB

import Network (HostName, PortID(..), connectTo)
import System.IO (Handle, hClose)

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
recv hdl p = io $ A.parseWith (SB.hGetSome hdl kBUFSIZ) p mempty

-- Feed input n octets at a time (TODO: what is a good size here?)
kBUFSIZ :: Int
kBUFSIZ = 1024

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
      A.Partial _  -> Left (ParseError   "insufficient input")

withConn :: HostName -> PortID -> (Handle -> IO a) -> IO a
withConn host port = bracket
  (do hdl <- connectTo host port
      _   <- A.parseOnly helo <$> SB.hGetLine hdl
      return hdl)
  (\hdl -> SB.hPut hdl "close\n" >> hClose hdl)
