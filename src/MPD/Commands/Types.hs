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
    Decibel
  , Metadata(..)
  , PlaybackState(..)
  , Seconds
  , SongId
  , SongPos
  , SubsystemName(..)
  , Volume
  , Path(..)
  , Range

    -- * Compound objects
    -- $object
  , LsEntry(..)
  , LsEntryInfo(..)
  , SongInfo(..)
  , StatusInfo(..)
  , StatsInfo(..)
  , viewTag

  , songFileL
  , songLastModifiedL
  , songTimeL
  , songTagsL
  , songPosL
  , songIdL
  , viewTagL
  ) where

import MPD.Core.CommandArg

import Control.Applicative (Applicative(..))
import Data.Monoid (Monoid(..))
import Control.DeepSeq (NFData(..))
import Data.Maybe (fromJust)
import Data.Monoid (Monoid(..), Sum(..), (<>))
import Data.String (IsString(..))
import Data.Data (Data, Typeable)
import Data.Time (UTCTime)
import Data.Text (Text)
import Data.ByteString.Char8 (ByteString)
import qualified Data.HashMap.Strict as M
import qualified Data.List as L

------------------------------------------------------------------------
-- $scalar

type Decibel = Double
type Seconds = Int
type SongId = Int
type SongPos = Int
type Volume = Int

newtype Path = Path { unPath :: Text } deriving (Show, Data, Typeable)

instance Monoid Path where
  mempty = Path ""
  p1 `mappend` p2 = Path (unPath p1 `mappend` unPath p2)

instance NFData Path where
  rnf (Path x) = rnf x

instance IsString Path where
  fromString = Path . fromString

instance CommandArg Path where
  fromArg (Path x) = "\"" <> x <> "\""

newtype Range = Range (Int, Maybe Int) deriving (Show, Data, Typeable)

instance Monoid Range where
  mempty = Range (0, Nothing)
  Range (s1, e1) `mappend` Range (s2, e2) = Range (s1 + s2, e3)
    where e3 = fmap getSum (fmap Sum e1 `mappend` fmap Sum e2)

instance NFData Range where
  rnf (Range x) = rnf x

instance CommandArg Range where
  fromArg (Range (a, b)) = fromArg a <> ":" <> fromArg b

data Metadata
  = Artist
  | ArtistSort
  | Album
  | AlbumArtist
  | AlbumArtistSort
  | Title
  | Track
  | Name
  | Genre
  | Date
  | Composer
  | Performer
  | Disc
  | MUSICBRAINZ_ARTISTID
  | MUSICBRAINZ_ALBUMID
  | MUSICBRAINZ_ALBUMARTISTID
  | MUSICBRAINZ_TRACKID
    deriving (Eq, Show, Read, Enum, Data, Typeable)

instance CommandArg Metadata where
  fromArg = fromString . show

data PlaybackState
  = PlaybackPlaying
  | PlaybackStopped
  | PlaybackPaused
    deriving (Eq, Show, Read, Enum, Data, Typeable)

instance NFData PlaybackState where
  rnf x = x `seq` ()

instance CommandArg PlaybackState where
  fromArg x = case x of
    PlaybackPlaying -> "play"
    PlaybackStopped -> "stop"
    PlaybackPaused  -> "pause"

data SubsystemName
  = Database
  | Player
  | Mixer
    deriving (Eq, Show, Read, Enum, Data, Typeable)

instance CommandArg SubsystemName where
  fromArg = fromJust . (`L.lookup` tbl)
    where
      tbl = [ (Database, "database")
            , (Player, "player")
            , (Mixer, "mixer")
            ]

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
  | LsPListInfo !Path !UTCTime
    deriving (Show, Data, Typeable)

instance NFData LsEntryInfo where
  rnf (LsSongInfo x) = rnf x
  rnf (LsDirInfo x y) = rnf x `seq` rnf y
  rnf (LsPListInfo x y) = rnf x `seq` rnf y

data StatsInfo = StatsInfo
  { statsArtists :: !Int
  , statsAlbums :: !Int
  , statsSongs :: !Int
  , statsUptime :: !Int
  , statsDbPlaytime :: !Int
  , statsDbUpdate :: !Int
  , statsPlaytime :: !Int
  } deriving (Show, Data, Typeable)

instance NFData StatsInfo where
  rnf x = rnf (statsArtists x) `seq`
          rnf (statsAlbums x) `seq`
          rnf (statsSongs x) `seq`
          rnf (statsUptime x) `seq`
          rnf (statsDbPlaytime x) `seq`
          rnf (statsDbUpdate x) `seq`
          rnf (statsPlaytime x)

data StatusInfo = StatusInfo
  { statusVolume :: !(Maybe Volume)
    -- ^ 'Just' the current volume (0-100) or 'Nothing' if missing mixer.
  , statusRepeatEnabled :: !Bool
  , statusRandomEnabled :: !Bool
  , statusSingleEnabled :: !Bool
  , statusConsumeEnabled :: !Bool
  , statusPlaylistVersion :: !Int
  , statusPlaylistLength :: !Int
  , statusMixrampDb :: !Decibel
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
  , songTags :: !(M.HashMap ByteString Text)
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

viewTag :: SongInfo -> ByteString -> Maybe Text
viewTag si l = M.lookup l (songTags si)

------------------------------------------------------------------------
-- Lenses

_Path :: (Functor f) => (Text -> f Text) -> Path -> f Path
_Path f s = fmap (\a -> Path a) (f (unPath s))

songFileL :: (Functor f) => (Path -> f Path) -> SongInfo -> f SongInfo
songFileL f s = fmap (\a -> s { songFile = a }) (f (songFile s))

songLastModifiedL
  :: (Functor f)
  => (UTCTime -> f UTCTime)
  -> SongInfo
  -> f SongInfo
songLastModifiedL f s =
  fmap (\a -> s { songLastModified = a }) (f (songLastModified s))

songTimeL
  :: (Functor f)
  => (Seconds -> f Seconds)
  -> SongInfo
  -> f SongInfo
songTimeL f s = fmap (\a -> s { songTime = a }) (f (songTime s))

songTagsL
  :: (Functor f)
  => (M.HashMap Label Text
  -> f (M.HashMap Label Text))
  -> SongInfo
  -> f SongInfo
songTagsL f s = fmap (\a -> s { songTags = a }) (f (songTags s))

songPosL
  :: (Functor f)
  => (Maybe SongPos -> f (Maybe SongPos))
  -> SongInfo
  -> f SongInfo
songPosL f s = fmap (\a -> s { songPos = a }) (f (songPos s))

songIdL
  :: (Functor f)
  => (Maybe SongId -> f (Maybe SongId))
  -> SongInfo
  -> f SongInfo
songIdL f s = fmap (\a -> s { songId = a }) (f (songId s))

viewTagL :: (Functor f) => Label -> (Maybe Text -> f (Maybe Text)) -> SongInfo -> f SongInfo
viewTagL lbl f s = fmap (\a -> case a of Just a' -> s { songTags = M.insert lbl a' (songTags s)  }
                                         _ -> s)
                        (f (M.lookup lbl (songTags s)))
