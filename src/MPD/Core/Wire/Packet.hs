{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe              #-}

{-|
Module      : MPD.Core.Wire.Packet
Copyright   : (c) Joachim Fasting, 2015

License     : MIT
Maintainer  : joachifm@fastmail.fm
Stability   : unstable
Portability : unportable
-}

module MPD.Core.Wire.Packet where

import qualified Data.ByteString.Char8 as SB
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.List as L

pack :: [T.Text] -> SB.ByteString
pack = T.encodeUtf8 . T.unlines
     . commandList
     . L.filter (not . T.null)

commandList :: [T.Text] -> [T.Text]
commandList []  = []
commandList [x] = [x]
commandList xs  = "command_list_ok_begin" : xs ++ ["command_list_end"]
