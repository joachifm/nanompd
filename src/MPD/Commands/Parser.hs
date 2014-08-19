{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}

{-|
Module      : MPD.Commands.Parser
Description : Protocol object parsers
Copyright   : (c) Joachim Fasting, 2014

License     : MIT
Maintainer  : joachifm@fastmail.fm
Stability   : unstable
Portability : unportable

Parsers for MPD protocol objects.
-}

module MPD.Commands.Parser (
    -- * Scalars
    -- $scalar
    dateP
  , pathP
    
    -- * Objects
    -- $object
  , lsEntry
  , lsEntryInfo
  , songInfo
  , statusInfo
  ) where

import MPD.Core
import MPD.Commands.Types
import Control.Applicative

------------------------------------------------------------------------
-- $scalar

dateP :: String -> Either String Date
dateP = textP

pathP :: String -> Either String Path
pathP = Right . Path

------------------------------------------------------------------------
-- $object

lsEntry :: Parser LsEntry
lsEntry =
  LsFile <$> field "file" pathP <|>
  LsDir <$> field "directory" pathP <|>
  LsPlaylist <$> field "playlist" pathP


lsEntryInfo :: Parser LsEntryInfo
lsEntryInfo =
  LsSongInfo <$> songInfo <|>
  (LsDirInfo <$> field "directory" pathP <*> field "Last-Modified" dateP) <|>
  (LsPlaylistInfo <$> field "playlist" pathP
                  <*> field "Last-Modified" dateP)


statusInfo :: Parser StatusInfo
statusInfo = StatusInfo <$>
  field "volume" intP <*>
  field "repeat" boolP <*>
  field "random" boolP <*>
  field "single" boolP <*>
  field "consume" boolP <*>
  field "playlist" intP <*>
  field "playlistlength" intP <*>
  field "mixrampdb" doubleP <*>
  field "state" textP <*>
  optional (field "song" intP) <*>
  optional (field "songid" intP) <*>
  optional (field "time" textP) <*>
  optional (field "elapsed" doubleP) <*>
  optional (field "bitrate" intP) <*>
  optional (field "audio" textP) <*>
  optional (field "nextsong" intP) <*>
  optional (field "nextsongid" intP)

songInfo :: Parser SongInfo
songInfo = SongInfo <$>
  field "file" pathP <*>
  field "Last-Modified" dateP <*>
  field "Time" intP <*>
  many songTag <*>
  optional (field "Pos" intP) <*>
  optional (field "Id" intP)

songTag :: Parser (Label, Text)
songTag = foldr1 (<|>) $ map (`fieldK` textP) tagTypes

tagTypes :: [Label]
tagTypes = [ "Artist", "Title", "Album" ]
