{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

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
import qualified Data.List       as L
import qualified Data.Text       as T
import qualified Data.Text.Read  as T

import Prelude hiding (repeat)

------------------------------------------------------------------------
-- Connection

ping :: Command ()
ping = command "ping" (return ())

------------------------------------------------------------------------
-- Current playlist

add :: T.Text -> Command ()
add uri = command ("add" .+ uri) (return ())

addId :: T.Text -> Maybe SongPos -> Command SongId
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

plChangesPosId :: Integer -> Command [(SB.ByteString, T.Text)]
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

listAllInfo :: Command [SongInfo]
listAllInfo = command "listallinfo" p
  where
    p = map songInfo . cyclesWith ("file" `SB.isPrefixOf`)

listAll :: T.Text -> Command [Either T.Text T.Text]
listAll meta = command ("listall" .+ meta) (L.foldl' p [])
  where
    p z x = case pair x of
      ("file", v) -> Right v : z
      (_,      v) -> Left v  : z

rescan :: Maybe T.Text -> Command Int
rescan mbPath = command ("rescan" .+ mbPath) p
  where
    p = either undefined fst . head . fmap (T.decimal . snd . pair) -- XXX: ugh

update :: Maybe T.Text -> Command Int
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
seekId sid time = command ("seekid" .+ sid .+ time) (return ())

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

setVolume :: Int -> Command ()
setVolume n = command ("setvol" .+ n) (return ())

single :: Bool -> Command ()
single b = command ("single" .+ b) (return ())

------------------------------------------------------------------------
-- Status

currentSong :: Command SongInfo
currentSong = command "currentsong" songInfo

idle :: [T.Text] -> Command [T.Text]
idle ss = command ("idle" .+ ss) (map (snd . pair))

noidle :: Command ()
noidle = command "noidle" (return ())

status :: Command StatusInfo
status = command "status" statusInfo

------------------------------------------------------------------------
-- Stored playlists

listPlaylistInfo :: T.Text -> Command [SongInfo]
listPlaylistInfo plName = command ("listplaylistinfo" .+ plName) p
  where
    p = map songInfo . cyclesWith ("file" `SB.isPrefixOf`)

load :: T.Text -> Command ()
load path = command ("load" .+ path) (return ())

save :: T.Text -> Command ()
save path = command ("save" .+ path) (return ())
