{-# LANGUAGE OverloadedStrings #-}

module MPD
  (
    Command
  , run
  , runWith

  , ping
  , status
  , currentsong
  , listallinfo
  ) where

import MPD.Core
import MPD.Util

import Control.DeepSeq (NFData(..), deepseq)

import qualified Data.List           as L
import qualified Data.HashMap.Strict as M

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.ByteString as SB

------------------------------------------------------------------------
-- MPD protocol command API.

ping :: Command ()
ping = Command ["ping"] (return ())

currentsong :: Command SongInfo
currentsong = Command ["currentsong"] (liftFold songInfo)

status :: Command StatusInfo
status = Command ["status"] (liftFold statusInfo)

listallinfo :: Command [SongInfo]
listallinfo = Command ["listallinfo"] (liftFold p)
  where p = map songInfo . cyclesWith ("file" `SB.isPrefixOf`)

------------------------------------------------------------------------
-- MPD protocol objects.

data StatusInfo = StatusInfo
  { statusVolume          :: {-# UNPACK #-} !Text
  , statusRepeatEnabled   :: {-# UNPACK #-} !Text
  , statusRandomEnabled   :: {-# UNPACK #-} !Text
  , statusSingleEnabled   :: {-# UNPACK #-} !Text
  , statusConsumeEnabled  :: {-# UNPACK #-} !Text
  , statusPlaylistVersion :: {-# UNPACK #-} !Text
  , statusPlaylistLength  :: {-# UNPACK #-} !Text
  , statusMixrampDb       :: {-# UNPACK #-} !Text
  , statusPlaybackState   :: {-# UNPACK #-} !Text
  , statusSongPos         :: {-# UNPACK #-} !Text
  , statusSongId          :: {-# UNPACK #-} !Text
  , statusTotalTime       :: {-# UNPACK #-} !Text
  , statusElapsedTime     :: {-# UNPACK #-} !Text
  , statusBitrate         :: {-# UNPACK #-} !Text
  , statusAudio           :: {-# UNPACK #-} !Text
  , statusNextSongPos     :: {-# UNPACK #-} !Text
  , statusNextSongId      :: {-# UNPACK #-} !Text
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

statusInfo :: [ByteString] -> StatusInfo
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
  { songFile :: {-# UNPACK #-} !Text
  , songId   :: {-# UNPACK #-} !Text
  , songPos  :: {-# UNPACK #-} !Text
  , songTags :: !(M.HashMap ByteString Text)
  } deriving (Show)

instance NFData SongInfo where
  rnf x = songFile x `deepseq`
          songId x   `deepseq`
          songPos x  `deepseq`
          songTags x `deepseq` ()

songInfo :: [ByteString] -> SongInfo
songInfo = L.foldl' step initial
  where
    step z x = case pair x of
      ("file", v) -> z { songFile = v }
      ("Id", v)   -> z { songId = v }
      ("Pos", v)  -> z { songPos = v }
      (k, v)      -> z { songTags = M.insert k v (songTags z) }

    initial = SongInfo
      { songFile = ""
      , songId = ""
      , songPos = ""
      , songTags = M.empty
      }
