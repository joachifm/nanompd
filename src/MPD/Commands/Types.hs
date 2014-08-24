{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE DeriveDataTypeable #-}
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
    Metadata(..)
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

import Control.DeepSeq (NFData(..))
import Data.Monoid ((<>))
import Data.String (IsString(..))
import Data.Data (Data, Typeable)
import Data.Time.Clock (UTCTime)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as M

------------------------------------------------------------------------
-- $scalar

type PlaybackState = Text
type Seconds = Int
type SongId = Int
type SongPos = Int
type SubsystemName = Text
type Volume = Int

newtype Path = Path { unPath :: Text } deriving (Show, Data, Typeable)

instance NFData Path where
  rnf (Path x) = rnf x

instance IsString Path where
  fromString = Path . T.pack

instance CommandArg Path where
  fromArg (Path x) = T.concat [ "\"", x, "\"" ]

newtype Range = Range (Int, Int) deriving (Show, Data, Typeable)

instance NFData Range where
  rnf (Range x) = rnf x

instance CommandArg Range where
  fromArg (Range (a, b)) = fromArg a <> ":" <> fromArg b

data Metadata = Artist | Title
  deriving (Eq, Show, Read, Enum, Data, Typeable)

instance CommandArg Metadata where
  fromArg = T.pack . show

------------------------------------------------------------------------
-- $object

data LsEntry
  = LsFile !Path
  | LsDir !Path
  | LsPlaylist !Path
    deriving (Show, Data, Typeable)

instance NFData LsEntry where
  rnf (LsFile x) = rnf x
  rnf (LsDir x) = rnf x
  rnf (LsPlaylist x) = rnf x

data LsEntryInfo
  = LsSongInfo !SongInfo
  | LsDirInfo !Path !UTCTime
  | LsPlaylistInfo !Path !UTCTime
    deriving (Show, Data, Typeable)    

instance NFData LsEntryInfo where
  rnf (LsSongInfo x) = rnf x
  rnf (LsDirInfo x y) = rnf x `seq` rnf y
  rnf (LsPlaylistInfo x y) = rnf x `seq` rnf y

data StatusInfo = StatusInfo
  { statusVolume :: !(Maybe Volume)
    -- ^ 'Just' the current volume (0-100) or 'Nothing' if missing mixer.
  , statusRepeatEnabled :: !Bool
  , statusRandomEnabled :: !Bool
  , statusSingleEnabled :: !Bool
  , statusConsumeEnabled :: !Bool
  , statusPlaylistVersion :: !Int
  , statusPlaylistLength :: !Int
  , statusMixrampDb :: !Double
  , statusPlaybackState :: !PlaybackState
  , statusSongPos :: !(Maybe SongPos)
  , statusSongId :: !(Maybe SongId)
  , statusTime :: !(Maybe (Seconds, Seconds))
  , statusElapsedTime :: !(Maybe Double)
  , statusBitrate :: !(Maybe Int)
  , statusAudio :: !(Maybe (Int, Int, Int))
  , statusNextSongPos :: !(Maybe SongPos)
  , statusNextSongId :: !(Maybe SongId)
  } deriving (Show, Data, Typeable)

instance NFData StatusInfo where
  rnf x = rnf (statusVolume x) `seq`
          rnf (statusRepeatEnabled x) `seq`
          rnf (statusRandomEnabled x) `seq`
          rnf (statusSingleEnabled x) `seq`
          rnf (statusConsumeEnabled x) `seq`
          rnf (statusPlaylistVersion x) `seq`
          rnf (statusPlaylistLength x) `seq`
          rnf (statusMixrampDb x) `seq`
          rnf (statusPlaybackState x) `seq`
          rnf (statusSongPos x) `seq`
          rnf (statusSongId x) `seq`
          rnf (statusTime x) `seq`
          rnf (statusElapsedTime x) `seq`
          rnf (statusBitrate x) `seq`
          rnf (statusAudio x) `seq`
          rnf (statusNextSongPos x) `seq`
          rnf (statusNextSongId x)

data SongInfo = SongInfo
  { songFile :: !Path
  , songLastModified :: !UTCTime
  , songTime :: !Seconds
  , songTags :: !(M.HashMap Label Text)
  , songPos :: !(Maybe SongPos)
  , songId :: !(Maybe SongId)
  } deriving (Show, Data, Typeable)

instance NFData SongInfo where
  rnf x = rnf (songFile x) `seq`
          rnf (songLastModified x) `seq`
          rnf (songTime x) `seq`
          rnf (songTags x) `seq`
          rnf (songPos x) `seq`
          rnf (songId x)

viewTag :: SongInfo -> Label -> Maybe Text
viewTag si l = M.lookup l (songTags si)
