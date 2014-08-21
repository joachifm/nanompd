{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}

{-|
Module      : MPD.Commands.Types
Copyright   : (c) Joachim Fasting, 2014

License     : MIT
Maintainer  : joachifm@fastmail.fm
Stability   : unstable
Portability : unportable

Encodings for MPD protocol objects.
-}

module MPD.Commands.Types (
    -- * Scalars
    -- $scalar
    Date
  , PlaybackState
  , Seconds
  , SongId
  , SongPos
  , SubsystemName
  , Volume
  , Path(..)
  , Range

    -- * Compound objects
    -- $object
  , LsEntry(..)
  , LsEntryInfo(..)
  , SongInfo(..)
  , StatusInfo(..)
  , viewTag
  ) where

import MPD.Core

import Data.Monoid ((<>))
import Data.String (IsString(..))
import qualified Data.Text as T

------------------------------------------------------------------------
-- $scalar

type PlaybackState = Text
type Seconds = Int
type SongId = Int
type SongPos = Int
type SubsystemName = Text
type Volume = Int

type Date = Text

newtype Path = Path { unPath :: Text } deriving (Show)

instance IsString Path where
  fromString = Path . T.pack

instance CommandArg Path where
  fromArg (Path x) = T.concat [ "\"", x, "\"" ]

newtype Range = Range (Int, Int)

instance CommandArg Range where
  fromArg (Range (a, b)) = fromArg a <> ":" <> fromArg b

------------------------------------------------------------------------
-- $object

data LsEntry
  = LsFile Path
  | LsDir Path
  | LsPlaylist Path
    deriving (Show)

data LsEntryInfo
  = LsSongInfo SongInfo
  | LsDirInfo Path Date
  | LsPlaylistInfo Path Date
    deriving (Show)

data StatusInfo = StatusInfo
  { statusVolume :: Volume
  , statusRepeatEnabled :: Bool
  , statusRandomEnabled :: Bool
  , statusSingleEnabled :: Bool
  , statusConsumeEnabled :: Bool
  , statusPlaylistVersion :: Int
  , statusPlaylistLength :: Int
  , statusMixrampDb :: Double
  , statusPlaybackState :: PlaybackState
  , statusSongPos :: Maybe SongPos
  , statusSongId :: Maybe SongId
  , statusTime :: Maybe (Int, Int)
  , statusElapsedTime :: Maybe Double
  , statusBitrate :: Maybe Int
  , statusAudio :: Maybe (Int, Int, Int)
  , statusNextSongPos :: Maybe SongPos
  , statusNextSongId :: Maybe SongId
  } deriving (Show)

data SongInfo = SongInfo
  { songFile :: Path
  , songLastModified :: Date
  , songTime :: Seconds
  , songTags :: [(Label, Text)]
  , songPos :: Maybe SongPos
  , songId :: Maybe SongId
  } deriving (Show)

viewTag :: SongInfo -> Label -> Maybe Text
viewTag si l = lookup l (songTags si)
