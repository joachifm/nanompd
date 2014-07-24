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
    -- ** Song information
  , SongInfo(..)
  , viewTag
  , songInfo

    -- ** Status information
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
  {-# INLINE fromLit #-}

instance ToLit Range where
  toLit = (\(from, to) -> Range <$> parseDecimal from
                                <*> parseDecimal (T.drop 1 to))
        . T.break (== ':')
  {-# INLINE toLit #-}

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
  , statusSongPos         :: !(Maybe T.Text)
  , statusSongId          :: !(Maybe T.Text)
  , statusTime            :: !(Maybe T.Text)
  , statusElapsedTime     :: !(Maybe T.Text)
  , statusBitrate         :: !(Maybe T.Text)
  , statusAudio           :: !(Maybe T.Text)
  , statusNextSongPos     :: !(Maybe T.Text)
  , statusNextSongId      :: !(Maybe T.Text)
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
          statusTime x            `deepseq`
          statusElapsedTime x     `deepseq`
          statusBitrate x         `deepseq`
          statusAudio x           `deepseq`
          statusNextSongPos       `deepseq`
          statusNextSongId        `deepseq` ()
  {-# INLINE rnf #-}

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
      ("song", v)           -> z { statusSongPos = Just v }
      ("songid", v)         -> z { statusSongId = Just v }
      ("time", v)           -> z { statusTime = Just v }
      ("elapsed", v)        -> z { statusElapsedTime = Just v }
      ("bitrate", v)        -> z { statusBitrate = Just v }
      ("audio", v)          -> z { statusAudio = Just v }
      ("nextsong", v)       -> z { statusNextSongPos = Just v }
      ("nextsongid", v)     -> z { statusNextSongId = Just v }
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
      , statusSongPos = Nothing
      , statusSongId = Nothing
      , statusTime = Nothing
      , statusElapsedTime = Nothing
      , statusBitrate = Nothing
      , statusAudio = Nothing
      , statusNextSongPos = Nothing
      , statusNextSongId = Nothing
      }

data SongInfo = SongInfo
  { songFile :: !T.Text
  , songLastModified :: !T.Text
  , songTime :: !T.Text
  , songTags :: !(M.HashMap SB.ByteString T.Text)
  , songPos :: !(Maybe T.Text)
  , songId :: !(Maybe T.Text)
  } deriving (Show)

instance NFData SongInfo where
  rnf x = rnf (songFile x) `seq`
          rnf (songLastModified x) `seq`
          rnf (songTime x) `seq`
          rnf (songTags x) `seq`
          rnf (songPos x) `seq`
          rnf (songId x) `seq` ()
  {-# INLINE rnf #-}

viewTag :: SongInfo -> SB.ByteString -> Maybe T.Text
x `viewTag` k = M.lookup k (songTags x)

songInfo :: [SB.ByteString] -> SongInfo
songInfo = L.foldl' step initial
  where
    step z x = case pair x of
      ("file", v) -> z { songFile = v }
      ("Last-Modified", v) -> z { songLastModified = v }
      ("Time", v) -> z { songTime = v }
      ("Pos", v)  -> z { songPos = Just v }
      ("Id", v)   -> z { songId = Just v }
      (k, v)      -> z { songTags = M.insert k v (songTags z) }

    initial = SongInfo
      { songFile = ""
      , songLastModified = ""
      , songTime = ""
      , songTags = M.empty
      , songPos = Nothing
      , songId = Nothing
      }
