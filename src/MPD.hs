{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy       #-}

{-|
Module      : MPD
Description : Scripting client interactions with MPD
Copyright   : (c) Joachim Fasting, 2014

License     : MIT
Maintainer  : joachifm@fastmail.fm
Stability   : unstable
Portability : unportable

This module exports types and functions for scripting
client interactions with a running instance of MPD, the
music player daemon.
-}

module MPD
  (
    -- * Usage
    -- $usage

    -- * Extending
    -- $extending

    -- * Basic client environment
    ClientT
  , ClientError
  , runClientT

    -- * Command interface
  , Command
  , command
  , run
  , runWith

    -- * Convenient syntax for protocol command strings
  , CommandStr
  , CommandArg(..)
  , (.+)
  , render

    -- * Line-oriented response parser
  , Parser
  , parse
  , field
  , boolP
  , textP
  , intP
  , doubleP

    -- * Protocol objects
  , Date
  , Label
  , Path
  , Plain
  , Seconds
  , SongId
  , SongPos
  , Volume
  , Range(..)
  , PlaybackState
  , SubsystemName
  , LsEntry(..)
  , LsEntryInfo(..)
  , SongInfo(..)
  , viewTag
  , StatusInfo(..)

    -- ** Internal parsers
  , lsEntry
  , lsEntryInfo
  , songInfo
  , statusInfo

    -- * Canned MPD protocol command wrappers
    -- $wrappers

    -- ** Connection
  , ping

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

    -- * Re-exports
  , HostName
  , PortID(..)
  ) where

import Control.Applicative
import Control.Arrow (second)
import Control.Error (EitherT, runEitherT, left, right)
import Control.Monad (MonadPlus(..), ap, unless)
import Control.Monad.State (State, evalState, get, put)
import Control.Monad.Trans (MonadIO(..))
import Data.Functor (void)
import Data.Monoid (Monoid(..))
import Data.String (IsString(..))
import Network (HostName, PortID(..), connectTo)
import Prelude hiding (repeat)
import System.IO (Handle, hGetLine, hPutStr, hPutStrLn, hClose)
import System.IO.Error (tryIOError)
import qualified Data.List as List

{-$usage

MPD protocol command wrappers have the type 'Command' and may
be composed into compound commands with 'Applicative'.
Command wrappers are executed with 'run'.

The following snippet produces a crude report of the
currently playing song and the daemon's status information

@
import qualified MPD

MPD.run ((,) \<$\> MPD.currentSong \<*\> MPD.status)
@
-}

{-$extending
Define new commands with 'command', a smart constructor which
takes a protocol command string and a parser for the response.
Consult the MPD specification for the syntax used by a particular
command.

Command parsers operate only on the response pertaining to a
particular command (modulo the command list delimiter).

Protocol objects are @key\/value@ pairs and are parsed into a
corresponding record structure by composing 'field' parsers.
See e.g., 'songInfo' and 'statusInfo'.

With @-XOverloadedStrings@, there is a convenient syntax for building
command strings:

@
foo = "command_name" .+ arg1 .+ arg2
@

where @arg1 .. argN@ are members of @CommandArg@.
-}

------------------------------------------------------------------------
-- A basic client environment.

data ClientError
  = ParseError String
  | ProtocolError String
  | InvalidHost
  | ConnError IOError
  | Custom String
    deriving (Show)

{-|
A basic environment for MPD clients, parameterised over
some base monad.
-}
type ClientT m a = EitherT ClientError m a

runClientT :: ClientT m a -> m (Either ClientError a)
runClientT = runEitherT

------------------------------------------------------------------------
-- Line-oriented response parser.

newtype Parser a = P { runP :: State [String] (Either String a) }
  deriving (Functor)

instance Monad Parser where
  return x = P $ return (Right x)
  f >>= k  = P $ do
    r <- runP f
    case r of
      Left e  -> return (Left e)
      Right x -> runP (k x)

instance MonadPlus Parser where
  mzero = P $ return (Left "")
  f `mplus` k = P $ do
    r <- runP f
    case r of
      Left _ -> runP k
      _      -> return r

instance Applicative Parser where
  pure  = return
  (<*>) = ap

instance Alternative Parser where
  empty = mzero
  (<|>) = mplus

parse :: Parser a -> [String] -> Either String a
parse p = evalState (runP p)

------------------------------------------------------------------------

textP :: String -> Either String String
textP = Right

boolP :: String -> Either String Bool
boolP "0" = Right False
boolP "1" = Right True
boolP i   = Left ("expected 0 or 1; got " ++ i)

readP :: (Read a) => String -> String -> Either String a
readP n i = case reads i of
  [(r, "")] -> Right r
  _         -> Left ("expected " ++ n ++ "; got " ++ i)

intP :: String -> Either String Int
intP i = readP "[0-9]" i

doubleP :: String -> Either String Double
doubleP i = readP "double" i

-- Convert value parser into a line parser (i.e., consumes an entire line)
liftP :: (String -> Either String a) -> Parser a
liftP p = P $ do
  st <- get
  case st of
    [x] -> return $ p x
    _   -> return (Left $ "liftP: empty input")

fieldK :: String -> (String -> Either String a) -> Parser (String, a)
fieldK k v = P $ do
  st <- get
  case st of
    []   -> return $ Left ("field " ++ k ++ ": empty input")
    l:ls -> case pair l of
      (k', v')
        | k' == k -> case v v' of
          Right x -> put ls >> return (Right (k, x))
          Left e  -> return (Left $ "field: parse value failed: " ++ e)
        | otherwise -> return (Left $ "field: expected key " ++ k ++ "; got " ++ k')
  where
    pair s = let (hd, tl) = break (== ':') s in (hd, drop 2 tl)

field :: String -> (String -> Either String a) -> Parser a
field k v = fmap snd (fieldK k v)

------------------------------------------------------------------------
-- Connection primitives.

connIO :: (MonadIO m) => IO a -> ClientT m a
connIO m = either (left . ConnError) return =<< liftIO (tryIOError m)

recv :: (MonadIO m) => Handle -> ClientT m [String]
recv hdl = go
  where
    go = do
      ln <- connIO (hGetLine hdl)
      if ln == "OK"
        then right []
      else if "ACK" `List.isPrefixOf` ln
        then left $ ProtocolError (List.drop 4 ln)
      else fmap (ln :) go

send :: (MonadIO m) => Handle -> [String] -> ClientT m ()
send hdl q = connIO (hPutStr hdl q')
  where
    q' = unlines $ "command_list_ok_begin" : q ++ ["command_list_end"]

getResponse :: (MonadIO m) => Handle -> [String] -> Parser a -> ClientT m a
getResponse hdl q p = do
  send hdl q
  either (left . ParseError) right =<< (parse p `fmap` recv hdl)

open :: (MonadIO m) => HostName -> PortID -> ClientT m (Handle, String)
open host port = do
  hdl <- connIO (connectTo host port)
  ver <- connIO (hGetLine hdl)
  unless ("OK MPD " `List.isPrefixOf` ver) $ do
    close hdl
    left InvalidHost
  return (hdl, ver)

close :: (MonadIO m) => Handle -> ClientT m ()
close hdl = void . liftIO $ tryIOError (hPutStr hdl "close\n" >> hClose hdl)

withConn
  :: (MonadIO m)
  => HostName
  -> PortID
  -> (Handle -> ClientT m a)
  -> ClientT m a
withConn host port f = do
  (hdl, _) <- open host port
  res <- f hdl
  close hdl
  return $! res

------------------------------------------------------------------------
-- Convenient syntax for protocol command strings.

data CommandStr = CommandStr [String]
  deriving (Show)

instance Monoid CommandStr where
  mempty = CommandStr []
  CommandStr a `mappend` CommandStr b = CommandStr (a `mappend` b)

instance IsString CommandStr where
  fromString x = CommandStr [x]

class CommandArg a where
  fromArg :: a -> String

instance (CommandArg a) => CommandArg (Maybe a) where
  fromArg m = maybe mempty fromArg m

instance (CommandArg a) => CommandArg [a] where
  fromArg m = unwords (map fromArg m)

instance CommandArg Int where
  fromArg x = show x

instance CommandArg Integer where
  fromArg x = show x

instance CommandArg Double where
  fromArg x = show x

instance CommandArg Bool where
  fromArg x = if x then "1" else "0"

(.+) :: (CommandArg a) => CommandStr -> a -> CommandStr
CommandStr s .+ a = CommandStr (s ++ [fromArg a])

render :: CommandStr -> String
render (CommandStr as) = unwords (filter (not . null) as)

------------------------------------------------------------------------
-- Applicative command interface.

data Command a = Command [CommandStr] (Parser a)
  deriving (Functor)

instance Applicative Command where
  pure x = Command [] (pure x)
  Command q1 p1 <*> Command q2 p2 = Command (q1 ++ q2) (p1 <*> p2)

command :: CommandStr -> Parser a -> Command a
command q p = Command [q] $ P $ do
  (hd, tl) <- second (List.drop 1) . List.break (== "list_OK") <$> get
  rv <- (put hd >> runP p)
  put tl
  return rv

runWith :: HostName -> PortID -> Command a -> ClientT IO a
runWith host port (Command q p) = withConn host port $ \hdl ->
  getResponse hdl (map render q) p

run :: Command a -> ClientT IO a
run = runWith "localhost" (PortNumber 6600)
{-# INLINE run #-}

------------------------------------------------------------------------
-- Protocol objects.

type Date = String
type Label = String
type Seconds = Int
type SongId = Int
type SongPos = Int
type Volume = Int
type SubsystemName = Plain
type PlaybackState = String

newtype Plain = Plain String deriving (Show)

instance IsString Plain where
  fromString = Plain

instance CommandArg Plain where
  fromArg (Plain x) = x

plainP :: String -> Either String Plain
plainP = Right . Plain

newtype Path = Path String deriving (Show)

instance IsString Path where
  fromString = Path

instance CommandArg Path where
  fromArg (Path x) = '"' : x ++ "\""

pathP :: String -> Either String Path
pathP = Right . Path

newtype Range = Range (Int, Int)

instance CommandArg Range where
  fromArg (Range (a, b)) = fromArg a ++ ":" ++ fromArg b

data LsEntry
  = LsFile Path
  | LsDir Path
  | LsPlaylist Path
    deriving (Show)

lsEntry :: Parser LsEntry
lsEntry =
  LsFile <$> field "file" pathP <|>
  LsDir <$> field "directory" pathP <|>
  LsPlaylist <$> field "playlist" pathP

data LsEntryInfo
  = LsSongInfo SongInfo
  | LsDirInfo Path Date
  | LsPlaylistInfo Path Date
    deriving (Show)

lsEntryInfo :: Parser LsEntryInfo
lsEntryInfo =
  LsSongInfo <$> songInfo <|>
  (LsDirInfo <$> field "directory" pathP <*> field "Last-Modified" textP) <|>
  (LsPlaylistInfo <$> field "playlist" pathP
                  <*> field "Last-Modified" textP)

data StatusInfo = StatusInfo
  { _statusVolume :: Volume
  , _statusRepeatEnabled :: Bool
  , _statusRandomEnabled :: Bool
  , _statusSingleEnabled :: Bool
  , _statusConsumeEnabled :: Bool
  , _statusPlaylistVersion :: Int
  , _statusPlaylistLength :: Int
  , _statusMixrampDb :: Double
  , _statusPlaybackState :: PlaybackState
  , _statusSongPos :: Maybe SongPos
  , _statusSongId :: Maybe SongId
  , _statusTime :: Maybe Plain -- XXX: current:total
  , _statusElapsedTime :: Maybe Double
  , _statusBitrate :: Maybe Int
  , _statusAudio :: Maybe Plain -- XXX: a:b:c
  , _statusNextSongPos :: Maybe SongPos
  , _statusNextSongId :: Maybe SongId
  } deriving (Show)

statusInfo :: Parser StatusInfo
statusInfo = StatusInfo <$>
  field "volume" intP <*>
  field "repeat" boolP <*>
  field "random" boolP <*>
  field "single" boolP <*>
  field "consume" boolP <*>
  field "playlist" intP <*>
  field "playlistlength" intP <*>
  field "mixrampdb" doubleP <*>
  field "state" textP <*>
  optional (field "song" intP) <*>
  optional (field "songid" intP) <*>
  optional (field "time" plainP) <*>
  optional (field "elapsed" doubleP) <*>
  optional (field "bitrate" intP) <*>
  optional (field "audio" plainP) <*>
  optional (field "nextsong" intP) <*>
  optional (field "nextsongid" intP)

data SongInfo = SongInfo
  { _songFile :: Path
  , _songLastModified :: Date
  , _songTime :: Seconds
  , _songTags :: [(Label, Plain)]
  , _songPos :: Maybe SongPos
  , _songId :: Maybe SongId
  } deriving (Show)

viewTag :: SongInfo -> Label -> Maybe Plain
viewTag si l = lookup l (_songTags si)

songInfo :: Parser SongInfo
songInfo = SongInfo <$>
  field "file" pathP <*>
  field "Last-Modified" textP <*>
  field "Time" intP <*>
  many songTag <*>
  optional (field "Pos" intP) <*>
  optional (field "Id" intP)

songTag :: Parser (Label, Plain)
songTag = foldr1 (<|>) $ map (\k -> fieldK k plainP) tagTypes

tagTypes :: [Label]
tagTypes = [ "Artist", "Title", "Album" ]

------------------------------------------------------------------------
-- $wrappers
--
-- Each command corresponds to an MPD protocol command.
--
-- Please refer to the MPD protocol specification for details on each
-- command.

-- Connection

ping :: Command ()
ping = command "ping" (return ())

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
plChangesPosId :: Int -> Command [(Label, Plain)]
plChangesPosId v = command ("plchangesposid" .+ v) (many p)
  where
    p = (,) <$> field "cpos" textP <*> field "id" plainP

-- | Shuffle current playlist.
shuffle :: Maybe Range -> Command ()
shuffle mbRange = command ("shuffle" .+ mbRange) (return ())

-- Database

-- | Find items where @meta = value@ exactly.
find :: Plain -> Plain -> Command [SongInfo]
find meta value = command ("find" .+ meta .+ value) (many songInfo)

-- | Recurisvely list information for items under path (or root).
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
rescan mbPath = command ("rescan" .+ mbPath) (liftP intP)

-- | Initiate update, optionally at given path.
update :: Maybe Path -> Command Int
update mbPath = command ("update" .+ mbPath) (liftP intP)

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

-- | Seek by song id.
seekId :: SongId -> Seconds -> Command ()
seekId sid dest = command ("seekid" .+ sid .+ dest) (return ())

-- | Stop playback.
stop :: Command ()
stop = command "stop" (return ())

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

-- Status

-- | Song information for currently playing song, if any.
currentSong :: Command (Maybe SongInfo)
currentSong = command "currentsong" (optional songInfo)

-- | Wait for changes in any of the given subsystems.
idle :: [SubsystemName] -> Command [SubsystemName]
idle ss = command ("idle" .+ ss) (many p)
  where
    p = field "changed" plainP

-- | Cancel 'idle'.
noidle :: Command ()
noidle = command "noidle" (return ())

-- | Daemon status information.
status :: Command StatusInfo
status = command "status" statusInfo

-- Stored playlists

-- | List song information for songs in a stored playlist.
listPlaylistInfo :: Path -> Command [SongInfo]
listPlaylistInfo plName = command ("listplaylistinfo" .+ plName) (many songInfo)

-- | Load playlist.
load :: Path -> Command ()
load path = command ("load" .+ path) (return ())

-- | Save current playlist.
save :: Path -> Command ()
save path = command ("save" .+ path) (return ())
