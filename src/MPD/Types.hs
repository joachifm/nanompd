{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module MPD.Types
  (
    -- * Scalars
    Range
  , Seconds
  , SongId
  , SongPos

    -- * MPD protocol objects
  , SongInfo(..)
  , songInfo
  , StatusInfo(..)
  , statusInfo
  ) where

import MPD.CommandStr (Lit, FromLit(..), ToLit(..))
import MPD.Util

import Control.Applicative
import Control.DeepSeq (NFData(..), deepseq)
import Data.Monoid
import Data.String

import qualified Data.List as L
import qualified Data.ByteString as SB
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T

------------------------------------------------------------------------
-- Scalars

type Seconds = Int
type SongId  = Int
type SongPos = Int

data Range = Range {-# UNPACK #-} !Int {-# UNPACK #-} !Int

instance NFData Range where
  rnf (Range a b) = a `seq` b `seq` ()
  {-# INLINE rnf #-}

instance IsString (Maybe Range) where
  fromString "" = Nothing
  fromString xs = Just $ fromString xs

instance IsString Range where
  fromString = (\(from, to) -> Range (read from) (read $ drop 1 to))
             . break (== ':')

instance FromLit Range where
  fromLit (Range a b) = fromLit a <> ":" <> fromLit b

instance ToLit Range where
  toLit = (\(from, to) -> Range <$> parseDecimal from
                                <*> parseDecimal (T.drop 1 to))
        . T.break (== ':')

------------------------------------------------------------------------
-- MPD protocol objects.

data StatusInfo = StatusInfo
  { statusVolume          :: {-# UNPACK #-} !T.Text
  , statusRepeatEnabled   :: {-# UNPACK #-} !T.Text
  , statusRandomEnabled   :: {-# UNPACK #-} !T.Text
  , statusSingleEnabled   :: {-# UNPACK #-} !T.Text
  , statusConsumeEnabled  :: {-# UNPACK #-} !T.Text
  , statusPlaylistVersion :: {-# UNPACK #-} !T.Text
  , statusPlaylistLength  :: {-# UNPACK #-} !T.Text
  , statusMixrampDb       :: {-# UNPACK #-} !T.Text
  , statusPlaybackState   :: {-# UNPACK #-} !T.Text
  , statusSongPos         :: {-# UNPACK #-} !T.Text
  , statusSongId          :: {-# UNPACK #-} !T.Text
  , statusTotalTime       :: {-# UNPACK #-} !T.Text
  , statusElapsedTime     :: {-# UNPACK #-} !T.Text
  , statusBitrate         :: {-# UNPACK #-} !T.Text
  , statusAudio           :: {-# UNPACK #-} !T.Text
  , statusNextSongPos     :: {-# UNPACK #-} !T.Text
  , statusNextSongId      :: {-# UNPACK #-} !T.Text
  } deriving (Show)

instance NFData StatusInfo where
  rnf x = statusVolume x          `deepseq`
          statusRepeatEnabled x   `deepseq`
          statusRandomEnabled x   `deepseq`
          statusSingleEnabled x   `deepseq`
          statusConsumeEnabled x  `deepseq`
          statusPlaylistVersion x `deepseq`
          statusPlaylistLength x  `deepseq`
          statusMixrampDb x       `deepseq`
          statusPlaybackState x   `deepseq`
          statusSongPos x         `deepseq`
          statusSongId x          `deepseq`
          statusTotalTime x       `deepseq`
          statusElapsedTime x     `deepseq`
          statusBitrate x         `deepseq`
          statusAudio x           `deepseq`
          statusNextSongPos       `deepseq`
          statusNextSongId        `deepseq` ()

statusInfo :: [SB.ByteString] -> StatusInfo
statusInfo = L.foldl' step initial
  where
    step z x = case pair x of
      ("volume", v)         -> z { statusVolume = v }
      ("repeat", v)         -> z { statusRepeatEnabled = v }
      ("random", v)         -> z { statusRandomEnabled = v }
      ("single", v)         -> z { statusSingleEnabled = v }
      ("consume", v)        -> z { statusConsumeEnabled = v }
      ("playlist", v)       -> z { statusPlaylistVersion = v }
      ("playlistlength", v) -> z { statusPlaylistLength = v }
      ("mixrampdb", v)      -> z { statusMixrampDb = v }
      ("state", v)          -> z { statusPlaybackState = v }
      ("song", v)           -> z { statusSongPos = v }
      ("songid", v)         -> z { statusSongId = v }
      ("time", v)           -> z { statusTotalTime = v }
      ("elapsed", v)        -> z { statusElapsedTime = v }
      ("bitrate", v)        -> z { statusBitrate = v }
      ("audio", v)          -> z { statusAudio = v }
      ("nextsong", v)       -> z { statusNextSongPos = v }
      ("nextsongid", v)     -> z { statusNextSongId = v }
      (_, _)                -> z

    initial = StatusInfo
      { statusVolume = ""
      , statusRepeatEnabled = ""
      , statusRandomEnabled = ""
      , statusSingleEnabled = ""
      , statusConsumeEnabled = ""
      , statusPlaylistVersion = ""
      , statusPlaylistLength = ""
      , statusMixrampDb = ""
      , statusPlaybackState = ""
      , statusSongPos = ""
      , statusSongId = ""
      , statusTotalTime = ""
      , statusElapsedTime = ""
      , statusBitrate = ""
      , statusAudio = ""
      , statusNextSongPos = ""
      , statusNextSongId = ""
      }

data SongInfo = SongInfo
  { songFile :: {-# UNPACK #-} !T.Text
  , songId   :: {-# UNPACK #-} !T.Text
  , songPos  :: {-# UNPACK #-} !T.Text
  , songLastModified :: {-# UNPACK #-} !T.Text
  , songTags :: !(M.HashMap SB.ByteString T.Text)
  } deriving (Show)

instance NFData SongInfo where
  rnf x = songFile x `deepseq`
          songId x   `deepseq`
          songPos x  `deepseq`
          songLastModified x `deepseq`
          songTags x `deepseq` ()

songInfo :: [SB.ByteString] -> SongInfo
songInfo = L.foldl' step initial
  where
    step z x = case pair x of
      ("file", v) -> z { songFile = v }
      ("Id", v)   -> z { songId = v }
      ("Pos", v)  -> z { songPos = v }
      ("Last-Modified", v) -> z { songLastModified = v }
      (k, v)      -> z { songTags = M.insert k v (songTags z) }

    initial = SongInfo
      { songFile = ""
      , songId = ""
      , songPos = ""
      , songLastModified = ""
      , songTags = M.empty
      }
