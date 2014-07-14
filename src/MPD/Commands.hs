{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module MPD.Commands
  (
    -- * MPD protocol commands
    -- ** Connection
    ping

    -- ** Current playlist
  , add
  , addId
  , clear
  , deleteId
  , playlistInfo
  , plChangesPosId
  , shuffle

    -- ** Playback control
  , next
  , play
  , playId
  , previous
  , seekId
  , stop

    -- ** Playback options
  , consume
  , random
  , repeat
  , setVolume
  , single

    -- ** Database
  , find
  , listAll
  , listAllInfo
  , rescan
  , update

    -- ** Stored playlists
  , listPlaylistInfo
  , load
  , save

    -- ** Status
  , currentSong
  , idle
  , noidle
  , status

    -- * MPD protocol objects
  , StatusInfo(..)
  , SongInfo(..)

  , Range
  , SongPos
  , SongId
  , Seconds
  ) where

import MPD.CommandStr (FromLit(..), ToLit(..), (.+))
import MPD.Core
import MPD.Util

import Control.Applicative

import Control.DeepSeq (NFData(..), deepseq)
import Data.Monoid

import qualified Data.List           as L
import qualified Data.HashMap.Strict as M

import Data.ByteString (ByteString)
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.ByteString as SB
import qualified Data.Text       as T
import qualified Data.Text.Read  as T

import Prelude hiding (repeat)

------------------------------------------------------------------------
-- MPD protocol command API.

-- Connection

ping :: Command ()
ping = Command ["ping"] (return ())

-- Current playlist

add :: T.Text -> Command ()
add uri = Command ["add" .+ uri] (return ())

addId :: T.Text -> Maybe SongPos -> Command SongId
addId uri pos = Command ["addid" .+ uri .+ pos] (liftFold p)
  where
    p = either undefined fst . head . fmap (T.decimal . snd . pair) -- XXX: ugh

clear :: Command ()
clear = Command ["clear"] (return ())

deleteId :: SongId -> Command ()
deleteId id' = Command ["deleteid" .+ id'] (return ())

playlistInfo :: Command [SongInfo]
playlistInfo = Command ["playlistinfo"] (liftFold p)
  where
    p = map songInfo . cyclesWith ("file" `SB.isPrefixOf`)

plChangesPosId :: Integer -> Command [(SB.ByteString, T.Text)]
plChangesPosId ver = Command ["plchangesposid" .+ ver] (liftFold p)
  where
    p = concatMap (map pair) . cyclesWith ("cpos" `SB.isPrefixOf`)

shuffle :: Maybe Range -> Command ()
shuffle mbRange = Command ["shuffle" .+ mbRange] (return ())

-- Database

find :: T.Text -> T.Text -> Command [SongInfo]
find meta value = Command ["find" .+ meta .+ value] (liftFold p)
  where
    p = map songInfo . cyclesWith ("file" `SB.isPrefixOf`)

listAllInfo :: Command [SongInfo]
listAllInfo = Command ["listallinfo"] (liftFold p)
  where
    p = map songInfo . cyclesWith ("file" `SB.isPrefixOf`)

listAll :: T.Text -> Command [Either T.Text T.Text]
listAll meta = Command ["listall" .+ meta] (liftFold $ L.foldl' p [])
  where
    p z x = case pair x of
      ("file", v) -> Right v : z
      (_,      v) -> Left v  : z

rescan :: Maybe T.Text -> Command Int
rescan mbPath = Command ["rescan" .+ mbPath] (liftFold p)
  where
    p = either undefined fst . head . fmap (T.decimal . snd . pair) -- XXX: ugh

update :: Maybe T.Text -> Command Int
update mbPath = Command ["update" .+ mbPath] (liftFold p)
  where
    p = either undefined fst . head . fmap (T.decimal . snd . pair) -- XXX: ugh

-- Playback control

type SongPos = Int
type SongId  = Int
type Seconds = Int

next :: Command ()
next = Command ["next"] (return ())

play :: Maybe SongPos -> Command ()
play pos = Command ["play" .+ pos] (return ())

playId :: Maybe SongId -> Command ()
playId sid = Command ["playid" .+ sid] (return ())

previous :: Command ()
previous = Command ["previous"] (return ())

seekId :: SongId -> Seconds -> Command ()
seekId sid time = Command ["seekid" .+ sid .+ time] (return ())

stop :: Command ()
stop = Command ["stop"] (return ())

-- Playback options

consume :: Bool -> Command ()
consume b = Command ["consume" .+ b] (return ())

random :: Bool -> Command ()
random b = Command ["random" .+ b] (return ())

repeat :: Bool -> Command ()
repeat b = Command ["repeat" .+ b] (return ())

setVolume :: Int -> Command ()
setVolume n = Command ["setvol" .+ n] (return ())

single :: Bool -> Command ()
single b = Command ["single" .+ b] (return ())

-- Status

currentSong :: Command SongInfo
currentSong = Command ["currentsong"] (liftFold songInfo)

idle :: [T.Text] -> Command [T.Text]
idle ss = Command ["idle" .+ ss] (liftFold p)
  where
    p = map (snd . pair)

noidle :: Command ()
noidle = Command ["noidle"] (return ())

status :: Command StatusInfo
status = Command ["status"] (liftFold statusInfo)

-- Stored playlists

listPlaylistInfo :: T.Text -> Command [SongInfo]
listPlaylistInfo plName = Command ["listplaylistinfo" .+ plName] (liftFold p)
  where
    p = map songInfo . cyclesWith ("file" `SB.isPrefixOf`)

load :: T.Text -> Command ()
load path = Command ["load" .+ path] (return ())

save :: T.Text -> Command ()
save path = Command ["save" .+ path] (return ())

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
  { songFile :: {-# UNPACK #-} !Text
  , songId   :: {-# UNPACK #-} !Text
  , songPos  :: {-# UNPACK #-} !Text
  , songLastModified :: {-# UNPACK #-} !Text
  , songTags :: !(M.HashMap ByteString Text)
  } deriving (Show)

instance NFData SongInfo where
  rnf x = songFile x `deepseq`
          songId x   `deepseq`
          songPos x  `deepseq`
          songLastModified x `deepseq`
          songTags x `deepseq` ()

songInfo :: [ByteString] -> SongInfo
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
