{-# OPTIONS_HADDOCK show-extensions #-}
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
  ) where

import MPD.Core
import MPD.Commands.Types

import Control.Applicative
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.HashMap.Strict as M

------------------------------------------------------------------------
-- $scalar

dateP :: A.Parser Date
dateP = textP

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
  LsFile <$> field "file" pathP <|>
  LsDir <$> field "directory" pathP <|>
  LsPlaylist <$> field "playlist" pathP

lsEntryInfo :: Parser LsEntryInfo
lsEntryInfo =
  LsSongInfo <$> songInfo <|>
  (LsDirInfo <$> field "directory" pathP
             <*> field "Last-Modified" dateP) <|>
  (LsPlaylistInfo <$> field "playlist" pathP
                  <*> field "Last-Modified" dateP)

statusInfo :: Parser StatusInfo
statusInfo = StatusInfo <$>
  field "volume" volumeP <*>
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
  optional (field "time" timeElapsedP) <*>
  optional (field "elapsed" doubleP) <*>
  optional (field "bitrate" intP) <*>
  optional (field "audio" audioP) <*>
  optional (field "nextsong" intP) <*>
  optional (field "nextsongid" intP)

songInfo :: Parser SongInfo
songInfo = SongInfo <$>
  field "file" pathP <*>
  field "Last-Modified" dateP <*>
  field "Time" intP <*>
  (M.fromList <$> many songTag) <*>
  optional (field "Pos" intP) <*>
  optional (field "Id" intP)

songTag :: Parser (Label, Text)
songTag = foldr1 (<|>) $ map (`pair` textP) tagTypes

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
