{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy       #-}

{-|
Module      : MPD.Commands
Copyright   : (c) Joachim Fasting
License     : MIT

Stability   : unstable
Portability : unportable

MPD protocol command wrappers.
-}

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
  , pause
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
  , lsInfo
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
  ) where

import MPD.CommandStr ((.+))
import MPD.Core (Command, command)
import MPD.Types
import MPD.Util

import qualified Data.ByteString as SB
import qualified Data.Text       as T
import qualified Data.Text.Read  as T

import Prelude hiding (repeat)

------------------------------------------------------------------------
-- Connection

ping :: Command ()
ping = command "ping" (return ())

------------------------------------------------------------------------
-- Current playlist

add :: Path -> Command ()
add uri = command ("add" .+ uri) (return ())

addId :: Path -> Maybe SongPos -> Command SongId
addId uri pos = command ("addid" .+ uri .+ pos) p
  where
    p = either undefined fst . head . fmap (T.decimal . snd . pair) -- XXX: ugh

clear :: Command ()
clear = command "clear" (return ())

deleteId :: SongId -> Command ()
deleteId id' = command ("deleteid" .+ id') (return ())

playlistInfo :: Command [SongInfo]
playlistInfo = command "playlistinfo" p
  where
    p = map songInfo . cyclesWith ("file" `SB.isPrefixOf`)

plChangesPosId :: Integer -> Command [(Label, T.Text)]
plChangesPosId ver = command ("plchangesposid" .+ ver) p
  where
    p = concatMap (map pair) . cyclesWith ("cpos" `SB.isPrefixOf`)

shuffle :: Maybe Range -> Command ()
shuffle mbRange = command ("shuffle" .+ mbRange) (return ())

------------------------------------------------------------------------
-- Database

find :: T.Text -> T.Text -> Command [SongInfo]
find meta value = command ("find" .+ meta .+ value) p
  where
    p = map songInfo . cyclesWith ("file" `SB.isPrefixOf`)

listAllInfo :: Maybe Path -> Command [LsEntryInfo]
listAllInfo mbPath = command ("listallinfo" .+ mbPath) lsEntryInfo

listAll :: Path -> Command [LsEntry]
listAll path = command ("listall" .+ path) lsEntry

lsInfo :: Maybe Path -> Command [LsEntryInfo]
lsInfo mbPath = command ("lsinfo" .+ mbPath) lsEntryInfo

rescan :: Maybe Path -> Command Int
rescan mbPath = command ("rescan" .+ mbPath) p
  where
    p = either undefined fst . head . fmap (T.decimal . snd . pair) -- XXX: ugh

update :: Maybe Path -> Command Int
update mbPath = command ("update" .+ mbPath) p
  where
    p = either undefined fst . head . fmap (T.decimal . snd . pair) -- XXX: ugh

------------------------------------------------------------------------
-- Playback control

next :: Command ()
next = command "next" (return ())

pause :: Command ()
pause = command "pause" (return ())

play :: Maybe SongPos -> Command ()
play pos = command ("play" .+ pos) (return ())

playId :: Maybe SongId -> Command ()
playId sid = command ("playid" .+ sid) (return ())

previous :: Command ()
previous = command "previous" (return ())

seekId :: SongId -> Seconds -> Command ()
seekId sid dest = command ("seekid" .+ sid .+ dest) (return ())

stop :: Command ()
stop = command "stop" (return ())

------------------------------------------------------------------------
-- Playback options

consume :: Bool -> Command ()
consume b = command ("consume" .+ b) (return ())

random :: Bool -> Command ()
random b = command ("random" .+ b) (return ())

repeat :: Bool -> Command ()
repeat b = command ("repeat" .+ b) (return ())

setVolume :: Volume -> Command ()
setVolume n = command ("setvol" .+ n) (return ())

single :: Bool -> Command ()
single b = command ("single" .+ b) (return ())

------------------------------------------------------------------------
-- Status

currentSong :: Command SongInfo
currentSong = command "currentsong" songInfo

idle :: [SubsystemName] -> Command [SubsystemName]
idle ss = command ("idle" .+ ss) (map (snd . pair))

noidle :: Command ()
noidle = command "noidle" (return ())

status :: Command StatusInfo
status = command "status" statusInfo

------------------------------------------------------------------------
-- Stored playlists

listPlaylistInfo :: Path -> Command [SongInfo]
listPlaylistInfo plName = command ("listplaylistinfo" .+ plName) p
  where
    p = map songInfo . cyclesWith ("file" `SB.isPrefixOf`)

load :: Path -> Command ()
load path = command ("load" .+ path) (return ())

save :: Path -> Command ()
save path = command ("save" .+ path) (return ())
