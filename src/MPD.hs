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

Types and functions for scripting client interactions with a running instance
of MPD, the music player daemon.
-}

module MPD
  (
    -- * Usage
    -- $usage

    -- * Extending
    -- $extending

    -- * Client environment
    -- $env
    ClientError(..)

    -- * Command interface
    -- $command
  , Command
  , command
  , run
  , runWith

    -- * Connection primitives
    -- $connection
  , withConn
  , getResponse

    -- ** Internals
  , send
  , recv

    -- * Convenient syntax for protocol command strings
    -- $commandStr
  , CommandStr
  , CommandArg(..)
  , (.+)

    -- ** Internals
  , render

    -- * Command response parser
    -- $parser
  , Parser
  , parse

    -- ** Protocol object parser
  , field

    -- ** Protocol value parsers
  , boolP
  , textP
  , labelP
  , intP
  , doubleP

    -- * Protocol objects
    -- $objects

    -- ** Scalars
  , Date
  , Label
  , Path(..)
  , Text(..)
  , Seconds
  , SongId
  , SongPos
  , Volume
  , Range(..)
  , PlaybackState
  , SubsystemName

    -- ** Compound objects
  , LsEntry(..)
  , LsEntryInfo(..)
  , SongInfo(..)
  , viewTag
  , StatusInfo(..)

    -- ** Object parsers
  , lsEntry
  , lsEntryInfo
  , songInfo
  , statusInfo

    -- * MPD protocol command wrappers
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

    -- ** From "Network"
  , HostName
  , PortID(..)

    -- ** From "Control.Applicative"
  , Applicative(..)

    -- ** From "Control.Error"
  , EitherT(runEitherT)

    -- ** From "Control.Monad.Trans"
  , MonadIO(..)
  ) where

import Control.Applicative
import Control.Arrow (second)
import Control.Error (EitherT(..), left, right)
import qualified Control.Monad.Catch as C
import Control.Monad (MonadPlus(..), ap, unless)
import Control.Monad.State (State, evalState, get, put)
import Control.Monad.Trans (MonadIO(..))
import Data.Functor (void)
import Data.Monoid (Monoid(..))
import Data.String (IsString(..))
import Network (HostName, PortID(..), connectTo)
import Prelude hiding (repeat)
import System.IO (Handle, hGetLine, hPutStr, hClose)
import System.IO.Error (tryIOError)
import qualified Data.List as List

{-$usage
The client API is structured around the 'Command' type, which
represents an MPD protocol command wrapper.
Values of type 'Command' support arbitrary composition into compound
commands, to be executed in batch.
This module provides ready-made wrappers that cover most of the MPD
protocol.
A regular user should not have to define their own wrappers, other
than by combining those already provided.

Command wrappers are turned into client actions (computations
against a running server) with 'run'.
All MPD client actions are executed within a @EitherT ClientError@ monad,
use 'runEitherT' to unwrap the result.
Currently, all 'Command's executed by 'run' acquire a separate connection
to the MPD server.

The following snippet produces a crude report of the currently playing song
and the daemon's status information.

@
import MPD

main = either (userError . show) print . runEitherT $
  run ((,) \<$\> currentSong \<*\> status)
@

Note that "MPD" re-exports most of what you'll need to be
productive.
-}

{-$extending
Define new commands with 'command', a smart constructor which
takes a protocol command string and a parser for the response.
Consult the MPD specification for the syntax used by a particular
command.
Name new wrappers by taking the camelCase of the protocol command
name.
Command wrappers defined with 'command' are \"primitive\" and should
do the least amount of work to provide a useful interface to a single
protocol command.

A compound command may be conceived of as a linked list of protocol command
strings and an associated parser for the response pertaining to that command.
The commands are sent to the server in batch, and the response is
consumed by applying each parser to the part of the response pertaining
to the corresponding command.

Command parsers are limited to the part of the overall response
which pertains to their associated protocol command string and so
should be implemented without regard for preceding or following commands.
The implementation ensures that command responses and parsers are paired up.
Currently, left-overs are silently discarded, though you could define a
parser whose only job is to fail if there is any input left to consume.

Protocol objects are @key\/value@ pairs and are parsed into a corresponding
record structure using 'field', as in

@
fooParser = (,) <\$\> field "key" valueParser <\*\> field "key" valueParser
@

Typically, each protocol object will have a corresponding record structure
and parser, both doing the least amount of work necessary to be useful.
Examples of object parsers are 'songInfo' and 'statusInfo'.

With @-XOverloadedStrings@, there is a convenient syntax for building
command strings:

@
foo = "command_name" .+ arg1 .+ arg2
@

where @arg1 .. argN@ are instances of 'CommandArg'.
The library provides 'CommandArg' instances for several standard types,
as well as instances for '[]', 'Maybe' (optional parameters),
and 'Either' (choice).
-}

------------------------------------------------------------------------
-- $env

data ClientError
  = ParseError String
  | ProtocolError String
  | InvalidHost
  | ConnError IOError
  | Custom String
    deriving (Show)

------------------------------------------------------------------------
-- $parser

newtype Parser a = P { runP :: State [String] (Either String a) }

instance Functor Parser where
  fmap f (P g) = P (fmap (fmap f) g)

instance Monad Parser where
  return x = P $ return (Right x)
  f >>= k  = P $ either (return . Left) (runP . k) =<< runP f
  fail     = P . return . Left

instance MonadPlus Parser where
  mzero = P $ return (Left "")
  f `mplus` k = P $ either (\_ -> runP k) (return . Right) =<< runP f

instance Applicative Parser where
  pure  = return
  (<*>) = ap

instance Alternative Parser where
  empty = mzero
  (<|>) = mplus

parse :: Parser a -> [String] -> Either String a
parse = evalState . runP

-- Value parsers

boolP :: String -> Either String Bool
boolP "0" = Right False
boolP "1" = Right True
boolP i   = Left ("expected 0 or 1; got " ++ i)

readP :: (Read a) => String -> String -> Either String a
readP n i = case reads i of
  [(r, "")] -> Right r
  _         -> Left ("expected " ++ n ++ "; got " ++ i)

intP :: String -> Either String Int
intP = readP "[0-9]"

doubleP :: String -> Either String Double
doubleP = readP "double"

-- Convert value parser into a line parser (i.e., consumes an entire line)
liftP :: (String -> Either String a) -> Parser a
liftP p = P $ do
  st <- get
  case st of
    [x] -> return $ p x
    _   -> return (Left "liftP: empty input")

-- Object parser

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
{-# INLINE field #-}

------------------------------------------------------------------------
-- $connection

connIO :: (MonadIO m) => IO a -> EitherT ClientError m a
connIO m = either (left . ConnError) return =<< liftIO (tryIOError m)

getResponse
  :: (MonadIO m)
  => Handle
  -> [String]
  -> Parser a
  -> EitherT ClientError m a
getResponse hdl q p = do
  send hdl q
  either (left . ParseError) right =<< (parse p `fmap` recv hdl)

send :: (MonadIO m) => Handle -> [String] -> EitherT ClientError m ()
send hdl = connIO . hPutStr hdl . (\xs -> unlines $ case xs of
  [x] -> [x]
  _   -> ("command_list_ok_begin" : xs) ++ ["command_list_end"])

recv :: (MonadIO m) => Handle -> EitherT ClientError m [String]
recv hdl = go
  where
    go = do
      ln <- connIO (hGetLine hdl)
      if ln == "OK"
        then right []
      else if "ACK" `List.isPrefixOf` ln
        then left $ ProtocolError (List.drop 4 ln)
      else fmap (ln :) go

withConn
  :: (C.MonadMask m, MonadIO m)
  => HostName
  -> PortID
  -> (Handle -> EitherT ClientError m a)
  -> EitherT ClientError m a
withConn host port m = EitherT $ C.bracket
  (runEitherT $ open host port)
  (\eh -> case eh of Left e         -> return (Left e)
                     Right (hdl, _) -> runEitherT (close hdl))
  (\eh -> case eh of Left e         -> return (Left e)
                     Right (hdl, _) -> runEitherT (m hdl))

open
  :: (MonadIO m)
  => HostName
  -> PortID
  -> EitherT ClientError m (Handle, String)
open host port = do
  hdl <- connIO (connectTo host port)
  ver <- connIO (hGetLine hdl)
  unless ("OK MPD " `List.isPrefixOf` ver) $ do
    close hdl
    left InvalidHost
  return (hdl, ver)

close :: (MonadIO m) => Handle -> EitherT ClientError m ()
close hdl = void . liftIO $
  tryIOError (hPutStr hdl "close\n" >> hClose hdl)

------------------------------------------------------------------------
-- $commandStr

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
  fromArg = maybe mempty fromArg

instance (CommandArg a, CommandArg b) => CommandArg (Either a b) where
  fromArg = either fromArg fromArg

instance (CommandArg a) => CommandArg [a] where
  fromArg m = unwords (map fromArg m)

instance CommandArg Int where
  fromArg = show

instance CommandArg Integer where
  fromArg = show

instance CommandArg Double where
  fromArg = show

instance CommandArg Bool where
  fromArg x = if x then "1" else "0"

(.+) :: (CommandArg a) => CommandStr -> a -> CommandStr
CommandStr s .+ a = CommandStr (s ++ [fromArg a])

render :: CommandStr -> String
render (CommandStr as) = unwords (filter (not . null) as)

------------------------------------------------------------------------
-- $command

data Command a = Command [CommandStr] (Parser a)
  deriving (Functor)

instance Applicative Command where
  pure x = Command [] (pure x)
  Command q1 p1 <*> Command q2 p2 = Command (q1 ++ q2) (p1 <*> p2)

command :: CommandStr -> Parser a -> Command a
command q p = Command [q] $ P $ do
  (hd, tl) <- second (List.drop 1) . List.break (== "list_OK") <$> get
  rv <- put hd >> runP p
  put tl
  return rv

runWith
  :: (C.MonadMask m, MonadIO m)
  => HostName
  -> PortID
  -> Command a
  -> EitherT ClientError m a
runWith host port (Command q p) = withConn host port $ \hdl ->
  getResponse hdl (map render q) p

run
  :: (C.MonadMask m, MonadIO m)
  => Command a
  -> EitherT ClientError m a
run = runWith "localhost" (PortNumber 6600)

------------------------------------------------------------------------
-- $objects

type Date = Text
type Label = String
type Seconds = Int
type SongId = Int
type SongPos = Int
type Volume = Int
type SubsystemName = Text
type PlaybackState = Text

newtype Text = Text { unText :: String } deriving (Show)

instance IsString Text where
  fromString = Text

instance CommandArg Text where
  fromArg (Text x) = x

textP :: String -> Either String Text
textP = Right . Text

labelP :: String -> Either String String
labelP = Right

newtype Path = Path { unPath :: String } deriving (Show)

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
  { statusVolume :: Volume
  , statusRepeatEnabled :: Bool
  , statusRandomEnabled :: Bool
  , statusSingleEnabled :: Bool
  , statusConsumeEnabled :: Bool
  , statusPlaylistVersion :: Int
  , statusPlaylistLength :: Int
  , statusMixrampDb :: Double
  , statusPlaybackState :: PlaybackState
  , statusSongPos :: Maybe SongPos
  , statusSongId :: Maybe SongId
  , statusTime :: Maybe Text -- XXX: current:total
  , statusElapsedTime :: Maybe Double
  , statusBitrate :: Maybe Int
  , statusAudio :: Maybe Text -- XXX: a:b:c
  , statusNextSongPos :: Maybe SongPos
  , statusNextSongId :: Maybe SongId
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
  optional (field "time" textP) <*>
  optional (field "elapsed" doubleP) <*>
  optional (field "bitrate" intP) <*>
  optional (field "audio" textP) <*>
  optional (field "nextsong" intP) <*>
  optional (field "nextsongid" intP)

data SongInfo = SongInfo
  { songFile :: Path
  , songLastModified :: Date
  , songTime :: Seconds
  , songTags :: [(Label, Text)]
  , songPos :: Maybe SongPos
  , songId :: Maybe SongId
  } deriving (Show)

viewTag :: SongInfo -> Label -> Maybe Text
viewTag si l = lookup l (songTags si)

songInfo :: Parser SongInfo
songInfo = SongInfo <$>
  field "file" pathP <*>
  field "Last-Modified" textP <*>
  field "Time" intP <*>
  many songTag <*>
  optional (field "Pos" intP) <*>
  optional (field "Id" intP)

songTag :: Parser (Label, Text)
songTag = foldr1 (<|>) $ map (`fieldK` textP) tagTypes

tagTypes :: [Label]
tagTypes = [ "Artist", "Title", "Album" ]

------------------------------------------------------------------------
-- $wrappers

-- Connection

-- | Ping daemon.
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
plChangesPosId :: Int -> Command [(Label, Text)]
plChangesPosId v = command ("plchangesposid" .+ v) (many p)
  where
    p = (,) <$> field "cpos" labelP <*> field "id" textP

-- | Shuffle current playlist.
shuffle :: Maybe Range -> Command ()
shuffle mbRange = command ("shuffle" .+ mbRange) (return ())

-- Database

-- | Find items where @meta = value@ exactly.
find :: Text -> Text -> Command [SongInfo]
find meta value = command ("find" .+ meta .+ value) (many songInfo)

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
    p = field "changed" textP

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
