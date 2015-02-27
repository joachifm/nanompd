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
  , subsystemP
  , playbackStateP

    -- * Objects
    -- $object
  , lsEntryP
  , lsEntryInfoP
  , songInfoP
  , playlistSongInfoP
  , statusInfoP
  , statsInfoP
  , songTagP
  ) where

import MPD.Core
import MPD.Commands.Types

import Control.Applicative
import qualified Data.Attoparsec.ByteString.Char8 as A

import Data.ByteString (ByteString)
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
volumeP = Nothing <$ "-1" <|> Just <$> intP

timeElapsedP :: A.Parser (Int, Int)
timeElapsedP = (,) <$> A.decimal <* A.char ':' <*> A.decimal

audioP :: A.Parser (Int, Int, Int)
audioP = (,,) <$> A.decimal <* A.char ':'
              <*> A.decimal <* A.char ':'
              <*> A.decimal

subsystemP :: A.Parser SubsystemName
subsystemP =
  Database <$ "database" <|>
  Player   <$ "player"   <|>
  Mixer    <$ "mixer"

playbackStateP :: A.Parser PlaybackState
playbackStateP = PlaybackPlaying <$ "play" <|>
                 PlaybackStopped <$ "stop" <|>
                 PlaybackPaused  <$ "pause"

------------------------------------------------------------------------
-- $object

lsEntryP :: A.Parser LsEntry
lsEntryP =
  LsFile <$> fieldP "file" pathP <|>
  LsDir <$> fieldP "directory" pathP <|>
  LsPlaylist <$> fieldP "playlist" pathP

lsEntryInfoP :: A.Parser LsEntryInfo
lsEntryInfoP =
     LsSongInfo <$> songInfoP
 <|> (LsDirInfo
      <$> fieldP "directory" pathP
      <*> fieldP "Last-Modified" dateP)
 <|> (LsPListInfo
      <$> fieldP "playlist" pathP
      <*> fieldP "Last-Modified" dateP)

-- Note: sometimes MPD returns fields in a different order; see
-- playlistSongInfoP for details.
songInfoP :: A.Parser SongInfo
songInfoP = SongInfo
  <$> fieldP "file" pathP
  <*> fieldP "Last-Modified" dateP
  <*> fieldP "Time" intP
  <*> (M.fromList <$> A.many' songTagP)
  <*> optional (fieldP "Pos" intP)
  <*> optional (fieldP "Id" intP)

statusInfoP :: A.Parser StatusInfo
statusInfoP = StatusInfo <$>
  fieldP "volume" volumeP <*>
  fieldP "repeat" boolP <*>
  fieldP "random" boolP <*>
  fieldP "single" boolP <*>
  fieldP "consume" boolP <*>
  fieldP "playlist" intP <*>
  fieldP "playlistlength" intP <*>
  fieldP "mixrampdb" floatP <*>
  fieldP "state" playbackStateP <*>
  optional (fieldP "song" intP) <*>
  optional (fieldP "songid" intP) <*>
  optional (fieldP "time" timeElapsedP) <*>
  optional (fieldP "elapsed" floatP) <*>
  optional (fieldP "bitrate" intP) <*>
  optional (fieldP "audio" audioP) <*>
  optional (fieldP "nextsong" intP) <*>
  optional (fieldP "nextsongid" intP)

statsInfoP :: A.Parser StatsInfo
statsInfoP = StatsInfo <$>
  fieldP "uptime" intP <*>
  fieldP "playtime" intP <*>
  fieldP "artists" intP <*>
  fieldP "albums" intP <*>
  fieldP "songs" intP <*>
  fieldP "db_playtime" intP <*>
  fieldP "db_update" intP

playlistSongInfoP :: A.Parser SongInfo
playlistSongInfoP = adapt <$>
  fieldP "file" pathP <*>
  fieldP "Last-Modified" dateP <*>
  -- NOTE: unlike for lsallinfo, MPD returns the tags BEFORE
  -- time here, which is why we cannot re-use the parser in
  -- lsEntryInfoP.
  --
  -- TODO: rename current SongInfo to SongInfo' and provide
  -- newtype wrappers so we can distinguish between these at the
  -- type level as well.
  (M.fromList <$> A.many' songTagP) <*>
  fieldP "Time" intP <*>
  optional (fieldP "Pos" intP) <*>
  optional (fieldP "Id" intP)
  where
    adapt a b c d e f = SongInfo a b d c e f

songTagP :: A.Parser (ByteString, Text)
songTagP = A.choice (map (`pairP` textP) tagTypes)

tagTypes :: [ByteString]
tagTypes =
    [
      "Album"
    , "Artist"
    , "Date"
    , "Genre"
    , "Title"

    , "Track"
    , "Disc"

    , "Composer"
    , "Performer"

    , "AlbumArtist"
    , "AlbumArtistSort"
    , "AlbumSort"
    , "ArtistSort"

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
