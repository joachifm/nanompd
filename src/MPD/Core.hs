{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}

module MPD.Core
  (
    Command
  , command
  , run
  , runWith
#ifdef TESTING
  , commandRes
  , pack
  , response
#endif
  ) where

import MPD.CommandStr (CommandStr, render)

import Control.Applicative (Applicative(..), (<$>))
import Control.Arrow ((***))
import Control.Exception (bracket)
import Control.Monad (unless)
import Control.Monad.State.Strict (State, evalState, get, put)

import qualified Data.ByteString      as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T

import Network (HostName, PortID(..), connectTo)
import qualified System.IO       as IO
import qualified System.IO.Error as IO

------------------------------------------------------------------------

data Command a = Command [CommandStr] (State [SB.ByteString] a)
  deriving (Functor)

instance Applicative Command where
  pure x = Command [] (pure x)
  {-# INLINE pure #-}

  Command q1 p1 <*> Command q2 p2 = Command (q1 ++ q2) (p1 <*> p2)
  {-# INLINE (<*>) #-}

command :: CommandStr -> ([SB.ByteString] -> a) -> Command a
command q f = Command [q] $ do
  (a, res) <- (f *** drop 1) . break (== "list_OK") <$> get
  put res
  return a

#ifdef TESTING
commandRes :: Command a -> State [SB.ByteString] a
commandRes (Command _ p) = p
#endif

------------------------------------------------------------------------

run :: Command a -> IO a
run = runWith "localhost" (PortNumber 6600)

runWith :: HostName -> PortID -> Command a -> IO a
runWith host port (Command q p) = bracket open close $ \hdl -> do
  SB.hPut hdl (pack $ map render q)
  !res <- response `fmap` LB.hGetContents hdl
  either (fail . T.unpack) return (evalState p `fmap` res)
  where
    open = do
      hdl <- connectTo host port
      IO.hSetNewlineMode hdl IO.noNewlineTranslation
      IO.hSetEncoding hdl IO.utf8
      IO.hSetBuffering hdl IO.LineBuffering
      helo <- SB.hGetLine hdl
      unless ("OK MPD" `SB.isPrefixOf` helo) $
        fail "runWith: host failed to identify as MPD"
      return hdl

    close hdl = IO.tryIOError (SB.hPut hdl "close\n") >> IO.hClose hdl

pack :: [T.Text] -> SB.ByteString
pack = SB.concat
     . map ((`SB.snoc` 10) . T.encodeUtf8)
     . (++ ["command_list_end"]) . ("command_list_ok_begin" :)

------------------------------------------------------------------------

response :: LB.ByteString -> Either T.Text [SB.ByteString]
response = step . map LB.toStrict . LB.split 10
  where
    step (hd:tl)
      | hd == "OK"               = Right []
      | "ACK" `SB.isPrefixOf` hd = Left . T.decodeUtf8 $ SB.drop 4 hd
      | otherwise                = fmap (hd :) (step tl)
    step [] = Left "premature end-of-input"
