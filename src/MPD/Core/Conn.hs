{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}

{-|
Module      : MPD.Core.Conn
Copyright   : (c) Joachim Fasting, 2014

License     : MIT
Maintainer  : joachifm@fastmail.fm
Stability   : unstable
Portability : unportable
-}

module MPD.Core.Conn
  ( getResponse
  , withConn
  ) where

import MPD.Core.ClientError
import MPD.Core.CommandStr
import MPD.Core.Parser

import Control.Applicative
import Control.Monad (unless, void)
import Control.Monad.Trans (MonadIO(..))
import Control.Monad.Trans.Either (EitherT(..), left, right)
import qualified Control.Monad.Catch as C
import qualified GHC.IO.Exception as GE

import System.IO (Handle, hClose)
import System.IO.Error (tryIOError)

import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString as SB
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Network (HostName, PortID(..), connectTo)

------------------------------------------------------------------------
-- Sending requests and receiving responses

getResponse
  :: (C.MonadThrow m, MonadIO m)
  => Handle
  -> [CommandStr]
  -> Parser a
  -> EitherT ClientError m a
getResponse hdl q p = do
  send hdl q
  either (left . ParseError) right =<< parse p <$> recv hdl

send
  :: (C.MonadThrow m, MonadIO m)
  => Handle
  -> [CommandStr]
  -> EitherT ClientError m ()
send hdl = connIO . SB.hPut hdl . pack

pack :: [CommandStr] -> SB.ByteString
pack = T.encodeUtf8 . T.unlines . f . map render
  where
    f [x] = [x]
    f xs  = ("command_list_ok_begin" : xs) ++ ["command_list_end"]

recv
  :: (C.MonadThrow m, MonadIO m)
  => Handle
  -> EitherT ClientError m [SB.ByteString]
recv hdl = go
  where
    go = do
      ln <- connIO (SB.hGetLine hdl)
      if ln == "OK" then right []
      else if "ACK" `SB.isPrefixOf` ln then left .
        readProtocolError $ SB.drop 4 ln
      else fmap (ln :) go

------------------------------------------------------------------------
-- Acquire connection

withConn
  :: (C.MonadMask m, MonadIO m)
  => HostName
  -> PortID
  -> (Handle -> EitherT ClientError m a)
  -> EitherT ClientError m a
withConn h p = EitherT . C.bracket (runEitherT $ open h p) (e close) . e
  where
    e f = either (return . Left) (runEitherT . f . fst)

open
  :: (C.MonadThrow m, MonadIO m)
  => HostName
  -> PortID
  -> EitherT ClientError m (Handle, SB.ByteString)
open host port = do
  hdl <- connIO (connectTo host port)
  ver <- connIO (SB.hGetLine hdl)
  unless ("OK MPD " `SB.isPrefixOf` ver) $ do
    _ <- liftIO (tryIOError $ hClose hdl)
    left InvalidHost
  return (hdl, ver)

close :: (C.MonadThrow m, MonadIO m) => Handle -> EitherT ClientError m ()
close hdl = void . liftIO $
  tryIOError (SB.hPut hdl "close\n") >> tryIOError (hClose hdl)

------------------------------------------------------------------------
-- Internal helpers

connIO :: (C.MonadThrow m, MonadIO m) => IO a -> EitherT ClientError m a
connIO m = either handler return =<< liftIO (tryIOError m)
  where
    handler eh | isConnError eh = left (ConnError eh)
               | otherwise      = C.throwM eh

    isConnError eh = GE.ioe_type eh
      `elem` [ GE.ResourceVanished, GE.EOF ]

readProtocolError :: SB.ByteString -> ClientError
readProtocolError = either ParseError id . A.parseOnly protocolError
  where
    protocolError = ProtocolError <$>
      (A.char '[' *> A.decimal <* A.char '@') <*>
      (A.decimal <* A.string "] {") <*>
      (T.decodeUtf8 <$> A.takeWhile1 (/= '}') <* A.string "} ") <*>
      textP
