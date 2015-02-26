{-# LANGUAGE OverloadedStrings #-}

module Core (
  connect, close,
  pack, send,
  ) where

import Network
import System.IO
import qualified Data.ByteString.Char8 as SB

connect = do
  h <- connectTo "localhost" (PortNumber 6600)
  l <- SB.hGetLine h
  case SB.splitAt 7 l of
    ("OK MPD ", v) -> return (h, v)
    _              -> fail "invalid host"

close h = SB.hPut h "close\n" >> hClose h

send h = SB.hPut h . pack

pack = SB.unlines
     . ("command_list_ok_begin" :)
     . (++ ["command_list_end"])
     . filter (not . SB.null)
