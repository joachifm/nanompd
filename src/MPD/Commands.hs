{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}

{-|
Module      : MPD.Commands
Description : Protocol command wrappers
Copyright   : (c) Joachim Fasting, 2014

License     : MIT
Maintainer  : joachifm@fastmail.fm
Stability   : unstable
Portability : unportable
-}

module MPD.Commands
  (
    -- * Connection
    ping

    -- * Current playlist
  , add
  , addId
  , clear
  , deleteId
  , playlistInfo
  , plChangesPosId
  , shuffle

    -- * Playback control
  , next
  , pause
  , play
  , playId
  , previous
  , seekCur
  , seekId
  , seek
  , stop

    -- * Playback options
  , consume
  , random
  , repeat
  , setVolume
  , single

    -- * Database
  , find
  , listAll
  , listAllInfo
  , lsInfo
  , rescan
  , update

    -- * Stored playlists
  , listPlaylistInfo
  , load
  , save

    -- * Status
  , currentSong
  , idle
  , noidle
  , status
  , stats

  , module MPD.Commands.Query
  , module MPD.Commands.Types
  ) where

import MPD.Core
import MPD.Commands.Query
import MPD.Commands.Parser
import MPD.Commands.Types

import Control.Applicative
import Prelude hiding (repeat)

import Data.Text (Text)

------------------------------------------------------------------------
-- Connection

-- | Ping daemon.
ping :: Command ()
ping = command "ping" (return ())

------------------------------------------------------------------------
-- Current playlist

-- | Add path to current playlist.
add :: Path -> Command ()
add uri = command ("add" .+ uri) (return ())

-- | Like 'add', return song id of newly added path.
addId :: Path -> Maybe SongPos -> Command SongId
addId uri pos = command ("addid" .+ uri .+ pos) (liftP intP)

-- | Clear current playlist.
clear :: Command ()
clear = command "clear" (return ())

-- | Delete by song id.
deleteId :: SongId -> Command ()
deleteId id' = command ("deleteid" .+ id') (return ())

-- | List song information for items in current playlist.
playlistInfo :: Command [SongInfo]
playlistInfo = command "playlistinfo" (many songInfo)

-- | List changes to the playlist since a given version.
plChangesPosId :: Int -> Command [(Text, Text)]
plChangesPosId v = command ("plchangesposid" .+ v) (many p)
  where
    p = (,) <$> field_ "cpos" textP <*> field_ "id" textP

-- | Shuffle current playlist.
shuffle :: Maybe Range -> Command ()
shuffle mbRange = command ("shuffle" .+ mbRange) (return ())

------------------------------------------------------------------------
-- Database

-- | Find items where @meta = value@ exactly.
find :: Query -> Command [SongInfo]
find qry = command ("find" .+ qry) (many songInfo)

-- | A recursive 'lsInfo'.
listAllInfo :: Maybe Path -> Command [LsEntryInfo]
listAllInfo mbPath = command ("listallinfo" .+ mbPath) (many lsEntryInfo)

-- | List names for items under path.
listAll :: Path -> Command [LsEntry]
listAll path = command ("listall" .+ path) (many lsEntry)

-- | List information for items under path.
lsInfo :: Maybe Path -> Command [LsEntryInfo]
lsInfo mbPath = command ("lsinfo" .+ mbPath) (many lsEntryInfo)

-- | Initiate rescan, optionally at given path.
rescan :: Maybe Path -> Command Int
rescan mbPath = command ("rescan" .+ mbPath) (field_ "updating_db" intP)

-- | Initiate update, optionally at given path.
update :: Maybe Path -> Command Int
update mbPath = command ("update" .+ mbPath) (field_ "updating_db" intP)

------------------------------------------------------------------------
-- Playback control

-- | Advance to next playlist item.
next :: Command ()
next = command "next" (return ())

-- | Pause playback.
pause :: Command ()
pause = command "pause" (return ())

-- | Start playback.
play :: Maybe SongPos -> Command ()
play pos = command ("play" .+ pos) (return ())

-- | Like 'play' but for song id.
playId :: Maybe SongId -> Command ()
playId sid = command ("playid" .+ sid) (return ())

-- | Previous playlist item.
previous :: Command ()
previous = command "previous" (return ())

-- | Seek by song position.
seek :: SongPos -> Seconds -> Command ()
seek pos dest = command ("seek" .+ pos .+ dest) (return ())

-- | Seek in current song.
seekCur :: Seconds -> Command ()
seekCur dest = command ("seekcur" .+ dest) (return ())

-- | Seek by song id.
seekId :: SongId -> Seconds -> Command ()
seekId sid dest = command ("seekid" .+ sid .+ dest) (return ())

-- | Stop playback.
stop :: Command ()
stop = command "stop" (return ())

------------------------------------------------------------------------
-- Playback options

-- | Enable consume mode.
consume :: Bool -> Command ()
consume b = command ("consume" .+ b) (return ())

-- | Enable random mode.
random :: Bool -> Command ()
random b = command ("random" .+ b) (return ())

-- | Enable repeat mode.
repeat :: Bool -> Command ()
repeat b = command ("repeat" .+ b) (return ())

-- | Set volume.
setVolume :: Volume -> Command ()
setVolume n = command ("setvol" .+ n) (return ())

-- | Enable single mode.
single :: Bool -> Command ()
single b = command ("single" .+ b) (return ())

------------------------------------------------------------------------
-- Status

-- | Song information for currently playing song, if any.
currentSong :: Command (Maybe SongInfo)
currentSong = command "currentsong" (optional songInfo)

-- | Wait for changes in any of the given subsystems.
idle :: [SubsystemName] -> Command [SubsystemName]
idle ss = command ("idle" .+ ss) (many (field_ "changed" p))
  where
    p = pure Database <* "database" <|>
        pure Player   <* "player" <|>
        pure Mixer    <* "mixer"

-- | Cancel 'idle'.
noidle :: Command ()
noidle = command "noidle" (return ())

-- | Daemon status information.
status :: Command StatusInfo
status = command "status" statusInfo

-- | Daemon statistics.
stats :: Command StatsInfo
stats = command "stats" statsInfo

------------------------------------------------------------------------
-- Stored playlists

-- | List song information for songs in a stored playlist.
listPlaylistInfo :: Path -> Command [SongInfo]
listPlaylistInfo plName =
  command ("listplaylistinfo" .+ plName) (many songInfo)

-- | Load playlist.
load :: Path -> Command ()
load path = command ("load" .+ path) (return ())

-- | Save current playlist.
save :: Path -> Command ()
save path = command ("save" .+ path) (return ())
