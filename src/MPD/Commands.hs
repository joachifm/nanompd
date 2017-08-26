{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}

{-|
Module      : MPD.Commands
Description : Protocol command wrappers
Copyright   : (c) Joachim Fasting, 2015

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
ping = command_ "ping"

------------------------------------------------------------------------
-- Current playlist

-- | Add path to current playlist.
add :: Path -> Command ()
add uri = command_ ("add" .+ uri)

-- | Like 'add', return song id of newly added path.
addId :: Path -> Maybe SongPos -> Command SongId
addId uri pos = command ("addid" .+ uri .+ pos) intP

-- | Clear current playlist.
clear :: Command ()
clear = command_ "clear"

-- | Delete by song id.
deleteId :: SongId -> Command ()
deleteId id' = command_ ("deleteid" .+ id')

-- | List song information for items in current playlist.
playlistInfo :: Command [SongInfo]
playlistInfo = command "playlistinfo" (many playlistSongInfoP)

-- | List changes to the playlist since a given version.
plChangesPosId :: Int -> Command [(Text, Text)]
plChangesPosId v = command ("plchangesposid" .+ v) (many p)
  where
    p = (,) <$> fieldP "cpos" textP <*> fieldP "id" textP

-- | Shuffle current playlist.
shuffle :: Maybe Range -> Command ()
shuffle mbRange = command_ ("shuffle" .+ mbRange)

------------------------------------------------------------------------
-- Database

-- | Find items where @meta = value@ exactly.
find :: Query -> Command [SongInfo]
find qry = command ("find" .+ qry) (many songInfoP)

-- | A recursive 'lsInfo'.
listAllInfo :: Maybe Path -> Command [LsEntryInfo]
listAllInfo mbPath = command ("listallinfo" .+ mbPath) (many lsEntryInfoP)

-- | List names for items under path.
listAll :: Path -> Command [LsEntry]
listAll path = command ("listall" .+ path) (many lsEntryP)

-- | List information for items under path.
lsInfo :: Maybe Path -> Command [LsEntryInfo]
lsInfo mbPath = command ("lsinfo" .+ mbPath) (many lsEntryInfoP)

-- | Initiate rescan, optionally at given path.
rescan :: Maybe Path -> Command Int
rescan mbPath = command ("rescan" .+ mbPath) (fieldP "updating_db" intP)

-- | Initiate update, optionally at given path.
update :: Maybe Path -> Command Int
update mbPath = command ("update" .+ mbPath) (fieldP "updating_db" intP)

------------------------------------------------------------------------
-- Playback control

-- | Advance to next playlist item.
next :: Command ()
next = command_ "next"

-- | Pause playback.
pause :: Command ()
pause = command_ "pause"

-- | Start playback.
play :: Maybe SongPos -> Command ()
play pos = command_ ("play" .+ pos)

-- | Like 'play' but for song id.
playId :: Maybe SongId -> Command ()
playId sid = command_ ("playid" .+ sid)

-- | Previous playlist item.
previous :: Command ()
previous = command_ "previous"

-- | Seek by song position.
seek :: SongPos -> Seconds -> Command ()
seek pos dest = command_ ("seek" .+ pos .+ dest)

-- | Seek in current song.
seekCur :: Seconds -> Command ()
seekCur dest = command_ ("seekcur" .+ dest)

-- | Seek by song id.
seekId :: SongId -> Seconds -> Command ()
seekId sid dest = command_ ("seekid" .+ sid .+ dest)

-- | Stop playback.
stop :: Command ()
stop = command_ "stop"

------------------------------------------------------------------------
-- Playback options

-- | Enable consume mode.
consume :: Bool -> Command ()
consume b = command_ ("consume" .+ b)

-- | Enable random mode.
random :: Bool -> Command ()
random b = command_ ("random" .+ b)

-- | Enable repeat mode.
repeat :: Bool -> Command ()
repeat b = command_ ("repeat" .+ b)

-- | Set volume.
setVolume :: Volume -> Command ()
setVolume n = command_ ("setvol" .+ n)

-- | Enable single mode.
single :: Bool -> Command ()
single b = command_ ("single" .+ b)

------------------------------------------------------------------------
-- Status

-- | Song information for currently playing song, if any.
currentSong :: Command (Maybe SongInfo)
currentSong = command "currentsong" (optional playlistSongInfoP)

-- | Wait for changes in any of the given subsystems.
idle :: [SubsystemName] -> Command [SubsystemName]
idle ss = command ("idle" .+ ss) (many (fieldP "changed" subsystemP))

-- | Cancel 'idle'.
noidle :: Command ()
noidle = command_ "noidle"

-- | Daemon status information.
status :: Command StatusInfo
status = command "status" statusInfoP

-- | Daemon statistics.
stats :: Command StatsInfo
stats = command "stats" statsInfoP

------------------------------------------------------------------------
-- Stored playlists

-- | List song information for songs in a stored playlist.
listPlaylistInfo :: Path -> Command [SongInfo]
listPlaylistInfo plName =
  command ("listplaylistinfo" .+ plName) (many songInfoP)

-- | Load playlist.
load :: Path -> Command ()
load path = command_ ("load" .+ path)

-- | Save current playlist.
save :: Path -> Command ()
save path = command_ ("save" .+ path)

------------------------------------------------------------------------
-- Internal

-- | A variant of 'command' that throws away the result.
command_ :: CommandStr -> Command ()
command_ s = command s (return ())
