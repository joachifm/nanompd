{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}

{-|
Module      : MPD.Commands.Parser
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
    audioP
  , dateP
  , pathP
  , timeElapsedP
  , volumeP
    
    -- * Objects
    -- $object
  , lsEntry
  , lsEntryInfo
  , songInfo
  , statusInfo
  , statsInfo

#ifdef TEST
  , parseIso8601_utc
#endif
  ) where

import MPD.Core
import MPD.Commands.Types

import Control.Applicative
import qualified Data.Attoparsec.ByteString.Char8 as A

import Data.Text (Text)
import Data.Time.Calendar (Day, fromGregorian)
import Data.Time.Clock (UTCTime(..), DiffTime, secondsToDiffTime)
import qualified Data.HashMap.Strict as M

------------------------------------------------------------------------
-- $scalar

dateP :: A.Parser UTCTime
dateP = parseIso8601_utc

pathP :: A.Parser Path
pathP = Path <$> textP

volumeP :: A.Parser (Maybe Volume)
volumeP = pure Nothing <* "-1" <|> Just <$> intP

timeElapsedP :: A.Parser (Int, Int)
timeElapsedP = (,) <$> A.decimal <* A.char ':' <*> A.decimal

audioP :: A.Parser (Int, Int, Int)
audioP = (,,) <$> A.decimal <* A.char ':'
              <*> A.decimal <* A.char ':'
              <*> A.decimal

------------------------------------------------------------------------
-- $object

lsEntry :: Parser LsEntry
lsEntry =
  LsFile <$> field_ "file" pathP <|>
  LsDir <$> field_ "directory" pathP <|>
  LsPlaylist <$> field_ "playlist" pathP

lsEntryInfo :: Parser LsEntryInfo
lsEntryInfo =
  LsSongInfo <$> songInfo <|>
  (LsDirInfo <$> field_ "directory" pathP
             <*> field_ "Last-Modified" dateP) <|>
  (LsPlaylistInfo <$> field_ "playlist" pathP
                  <*> field_ "Last-Modified" dateP)

statusInfo :: Parser StatusInfo
statusInfo = StatusInfo <$>
  field_ "volume" volumeP <*>
  field_ "repeat" boolP <*>
  field_ "random" boolP <*>
  field_ "single" boolP <*>
  field_ "consume" boolP <*>
  field_ "playlist" intP <*>
  field_ "playlistlength" intP <*>
  field_ "mixrampdb" doubleP <*>
  field_ "state" playbackStateP <*>
  optional (field_ "song" intP) <*>
  optional (field_ "songid" intP) <*>
  optional (field_ "time" timeElapsedP) <*>
  optional (field_ "elapsed" doubleP) <*>
  optional (field_ "bitrate" intP) <*>
  optional (field_ "audio" audioP) <*>
  optional (field_ "nextsong" intP) <*>
  optional (field_ "nextsongid" intP)

playbackStateP :: A.Parser PlaybackState
playbackStateP = pure PlaybackPlaying <* "play" <|>
                 pure PlaybackStopped <* "stop" <|>
                 pure PlaybackPaused  <* "pause"

statsInfo :: Parser StatsInfo
statsInfo = StatsInfo <$>
  field_ "uptime" intP <*>
  field_ "playtime" intP <*>
  field_ "artists" intP <*>
  field_ "albums" intP <*>
  field_ "songs" intP <*>
  field_ "db_playtime" intP <*>
  field_ "db_update" intP

songInfo :: Parser SongInfo
songInfo = SongInfo <$>
  field_ "file" pathP <*>
  field_ "Last-Modified" dateP <*>
  field_ "Time" intP <*>
  (M.fromList <$> many songTag) <*>
  optional (field_ "Pos" intP) <*>
  optional (field_ "Id" intP)

songTag :: Parser (Label, Text)
songTag = foldr1 (<|>) $ map (`field` textP) tagTypes

tagTypes :: [Label]
tagTypes = [
    "Artist"
  , "ArtistSort"
  , "Album"
  , "AlbumArtist"
  , "AlbumArtistSort"
  , "Title"
  , "Track"
  , "Name"
  , "Genre"
  , "Date"
  , "Composer"
  , "Performer"
  , "Disc"
  , "MUSICBRAINZ_ARTISTID"
  , "MUSICBRAINZ_ALBUMID"
  , "MUSICBRAINZ_ALBUMARTISTID"
  , "MUSICBRAINZ_TRACKID"
  ]

------------------------------------------------------------------------
-- A specialised ISO 8601 parser for dates in the <date>T<time>Z format,
-- aka. restricted to UTC times only.

parseIso8601_utc :: A.Parser UTCTime
parseIso8601_utc = UTCTime <$> parseDay  <* A.char 'T'
                           <*> parseTime <* A.char 'Z'

parseTime :: A.Parser DiffTime
parseTime = secondsToDiffTime . sum <$> sequence [
    (* 3600) <$> A.decimal <* A.char ':'
  , (*   60) <$> A.decimal <* A.char ':'
  , A.decimal
  ]

parseDay :: A.Parser Day
parseDay = fromGregorian <$> A.decimal <* A.char '-'
                         <*> A.decimal <* A.char '-'
                         <*> A.decimal
