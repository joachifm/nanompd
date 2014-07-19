{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : MPD.Types
Copyright   : (c) Joachim Fasting
License     : MIT

Stability   : unstable
Portability : unportable

Protocol scalars and objects.
-}

module MPD.Types
  (
    -- * Scalars
    Range
  , Seconds
  , SongId
  , SongPos

    -- * MPD protocol objects
  , SongInfo(..)
  , viewTag
  , songInfo
  , StatusInfo(..)
  , statusInfo
  ) where

import MPD.Lit
import MPD.Util

import Control.Applicative
import Control.DeepSeq (NFData(..), deepseq)
import Data.Monoid

import qualified Data.List as L
import qualified Data.ByteString as SB
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T

------------------------------------------------------------------------
-- Scalars

type Seconds = Int
type SongId  = Int
type SongPos = Int

data Range = Range !Int !Int

instance NFData Range where
  rnf (Range a b) = a `seq` b `seq` ()
  {-# INLINE rnf #-}

instance FromLit Range where
  fromLit (Range a b) = fromLit a <> ":" <> fromLit b

instance ToLit Range where
  toLit = (\(from, to) -> Range <$> parseDecimal from
                                <*> parseDecimal (T.drop 1 to))
        . T.break (== ':')

------------------------------------------------------------------------
-- MPD protocol objects.

data StatusInfo = StatusInfo
  { statusVolume          :: !T.Text
  , statusRepeatEnabled   :: !T.Text
  , statusRandomEnabled   :: !T.Text
  , statusSingleEnabled   :: !T.Text
  , statusConsumeEnabled  :: !T.Text
  , statusPlaylistVersion :: !T.Text
  , statusPlaylistLength  :: !T.Text
  , statusMixrampDb       :: !T.Text
  , statusPlaybackState   :: !T.Text
  , statusSongPos         :: !T.Text
  , statusSongId          :: !T.Text
  , statusTotalTime       :: !T.Text
  , statusElapsedTime     :: !T.Text
  , statusBitrate         :: !T.Text
  , statusAudio           :: !T.Text
  , statusNextSongPos     :: !T.Text
  , statusNextSongId      :: !T.Text
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
  { songFile :: !T.Text
  , songId   :: !T.Text
  , songPos  :: !T.Text
  , songLastModified :: !T.Text
  , songTags :: !(M.HashMap SB.ByteString T.Text)
  } deriving (Show)

instance NFData SongInfo where
  rnf x = songFile x `deepseq`
          songId x   `deepseq`
          songPos x  `deepseq`
          songLastModified x `deepseq`
          songTags x `deepseq` ()

viewTag :: SongInfo -> SB.ByteString -> Maybe T.Text
x `viewTag` k = M.lookup k (songTags x)

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
