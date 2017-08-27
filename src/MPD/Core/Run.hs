{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}

{-|
Module      : MPD.Core.Run
Description : Core definitions
Copyright   : (c) Joachim Fasting, 2015

License     : MIT
Maintainer  : joachifm@fastmail.fm
Stability   : unstable
Portability : unportable
-}

module MPD.Core.Run where

import MPD.Core.ClientError
import MPD.Core.Command
import MPD.Core.CommandStr
import MPD.Core.Wire

import Control.Exception (bracket)
import Control.Monad.Trans.Except
import Network (HostName, PortID(..), connectTo)
import System.IO (Handle, hClose)
import System.IO.Error (tryIOError)
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as SB

-- | Run a 'Command' against an existing MPD server connection.
run :: Handle -> Command a -> ExceptT ClientError IO a
run hdl (Command q p) = do
  send hdl q
  -- Note: The ugliness of this code indicates that this design is bad,
  -- but it kind of works for now.
  res <- recv hdl (runExceptT p)
  ExceptT $ return $
    case res of

      A.Done l r -> case r of

        Left e  -> case A.parseOnly protocolErrorP e of
          Left e'            -> Left (Custom e')
          Right (a, b, c, d) -> Left (ProtocolError a b c d)

        Right x
          | l == "OK\n" -> Right x
          | otherwise   -> Left (ParseError $ "left-overs: " ++ show l)

      A.Fail i _ e -> Left (ParseError $ "failed parsing " ++ show i ++ "; " ++ e)
      A.Partial _  -> Left (ParseError   "insufficient input")

-- | Run a computation against a named MPD server.
--
--
-- Example usage:
--
-- > withConn "localhost" 6600 (\conn -> run conn cmd)
withConn :: HostName -> PortID -> (Handle -> IO a) -> IO a
withConn host port = bracket
  (do hdl <- connectTo host port
      _   <- A.parseOnly heloP <$> SB.hGetLine hdl
      return hdl)
  (\hdl -> SB.hPut hdl "close\n" >> hClose hdl)

--
-- Internal
--

send :: Handle -> [CommandStr] -> ExceptT ClientError IO ()
send hdl = io . SB.hPut hdl . pack . map render

recv :: Handle -> A.Parser a -> ExceptT ClientError IO (A.Result a)
recv hdl p = io $ A.parseWith (SB.hGetSome hdl kBUFSIZ) p mempty

io :: IO a -> ExceptT ClientError IO a
io m = ExceptT $ either (Left . ConnError) Right <$> tryIOError m

-- Feed input @n@ octets at a time.
kBUFSIZ :: Int
kBUFSIZ = 4096 -- getconf PAGE_SIZE
