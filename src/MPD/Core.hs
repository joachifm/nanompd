{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}

{-|
Module      : MPD.Core
Description : Core definitions
Copyright   : (c) Joachim Fasting, 2014

License     : MIT
Maintainer  : joachifm@fastmail.fm
Stability   : unstable
Portability : unportable

This module defines types and functions for scripting client interactions
with MPD, mainly useful for users wishing to extend the command set.
-}

module MPD.Core
  (
    -- * Overview
    -- $overview
    
    -- * Extending
    -- $extending

    -- * Running commands
    -- $run
    ClientError(..)
  , run
  , runWith

    -- * Connection primitives
    -- $connection
  , withConn
  , getResponse

    -- * Command interface
    -- $command
  , Command
  , command

    -- * Convenient syntax for protocol command strings
    -- $commandStr
  , CommandStr
  , CommandArg(..)
  , (.+)
  , render

    -- * Response parser
    -- $parser
  , Parser
  , parse

    -- ** Scalars
  , textP
  , readP
  , boolP
  , intP
  , doubleP

    -- ** Objects
  , Label
  , fieldK
  , field
  , liftP

    -- * Re-exports

    -- ** From "Data.ByteString"
  , SB.ByteString

    -- ** From "Data.Text"
  , T.Text

    -- ** From "Network"
  , HostName
  , PortID(..)

    -- ** From "Control.Applicative"
  , Applicative(..)
  , (<$>)

    -- ** From "Control.Monad.Trans.Either"
  , EitherT(runEitherT)

    -- ** From "Control.Monad.Trans"
  , MonadIO(..)
  ) where

import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Char8 as A8
import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SB8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Control.Applicative
import Control.Arrow (second)
import Control.Monad.Trans.Either (EitherT(..), left, right)
import qualified Control.Monad.Catch as C
import Control.Monad (MonadPlus(..), ap, unless)
import Control.Monad.State (State, evalState, get, put)
import Control.Monad.Trans (MonadIO(..))
import Data.Functor (void)
import Data.Maybe (listToMaybe)
import Data.Monoid (Monoid(..))
import Data.String (IsString(..))
import Network (HostName, PortID(..), connectTo)
import System.IO (Handle, hClose)
import System.IO.Error (tryIOError)
import qualified Data.List as List

{-$overview
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
-- $parser

newtype Parser a = P { runP :: State [SB.ByteString] (Either String a) }

instance Functor Parser where
  fmap f (P g) = P (fmap (fmap f) g)

instance Monad Parser where
  return x = P $ return (Right x)
  f >>= k  = P $ either (return . Left) (runP . k) =<< runP f
  fail     = P . return . Left

instance MonadPlus Parser where
  mzero = P $ return (Left mempty)
  f `mplus` k = P $ either (\_ -> runP k) (return . Right) =<< runP f

instance Applicative Parser where
  pure  = return
  (<*>) = ap

instance Alternative Parser where
  empty = mzero
  (<|>) = mplus

parse :: Parser a -> [SB.ByteString] -> Either String a
parse = evalState . runP

-- Value parsers

type Label = SB.ByteString

textP :: A.Parser T.Text
textP = T.decodeUtf8 <$> A.takeByteString

readP :: (Read a) => A.Parser a
readP = do
  x <- A.takeByteString
  case reads (SB8.unpack x) of
   [(r, "")] -> return r
   _ -> mzero

boolP :: A.Parser Bool
boolP = pure True <* A8.char '1' <|> pure False <* A8.char '0'

intP :: A.Parser Int
intP = A8.decimal

doubleP :: A.Parser Double
doubleP = A8.double

-- Convert value parser into a line parser (i.e., consumes an entire line)
liftP :: A.Parser a -> Parser a
liftP p = P $ (maybe (Left "liftP: empty input") (A.parseOnly p) . listToMaybe) <$> get

-- Object parser

fieldK :: Label -> A.Parser a -> Parser (Label, a)
fieldK k v = P $ do
  st <- get
  case st of
   []   -> return $ Left "field: empty input"
   ln:ls -> do
     case pair ln of
      (k', v')
        | k' == k -> case A.parseOnly v v' of
          Right x -> put ls >> return (Right (k, x))
          Left e   -> return (Left $ "field: parse value failed: " ++ show e)
        | otherwise -> return (Left $ "field: expected key " ++ show k ++ "; got " ++ show k')
  where
    pair s = let (hd, tl) = SB8.break (== ':') s in (hd, SB.drop 2 tl)

field :: Label -> A.Parser a -> Parser a
field k v = fmap snd (fieldK k v)
{-# INLINE field #-}

------------------------------------------------------------------------
-- $commandStr

data CommandStr = CommandStr [T.Text]
  deriving (Show)

instance Monoid CommandStr where
  mempty = CommandStr []
  CommandStr a `mappend` CommandStr b = CommandStr (a `mappend` b)

instance IsString CommandStr where
  fromString x = CommandStr [T.pack x]

class CommandArg a where
  fromArg :: a -> T.Text

instance (CommandArg a) => CommandArg (Maybe a) where
  fromArg = maybe mempty fromArg

instance (CommandArg a, CommandArg b) => CommandArg (Either a b) where
  fromArg = either fromArg fromArg

instance (CommandArg a) => CommandArg [a] where
  fromArg m = T.unwords (map fromArg m)

instance CommandArg Int where
  fromArg = T.pack . show

instance CommandArg Integer where
  fromArg = T.pack . show

instance CommandArg Double where
  fromArg = T.pack . show

instance CommandArg Bool where
  fromArg x = if x then "1" else "0"

instance CommandArg T.Text where
  fromArg = id

(.+) :: (CommandArg a) => CommandStr -> a -> CommandStr
CommandStr s .+ a = CommandStr (s ++ [fromArg a])

render :: CommandStr -> T.Text
render (CommandStr as) = T.unwords (filter (not . T.null) as)

------------------------------------------------------------------------
-- $command

data Command a = Command [CommandStr] (Parser a)

instance Functor Command where
  fmap f (Command q p) = Command q (fmap f p)

instance Applicative Command where
  pure x = Command [] (pure x)
  Command q1 p1 <*> Command q2 p2 = Command (q1 ++ q2) (p1 <*> p2)

command :: CommandStr -> Parser a -> Command a
command q p = Command [q] $ P $ do
  (hd, tl) <- second (List.drop 1) . List.break (== "list_OK") <$> get
  rv <- put hd >> runP p
  put tl
  return rv

------------------------------------------------------------------------
-- $connection

connIO :: (MonadIO m) => IO a -> EitherT ClientError m a
connIO m = either (left . ConnError) return =<< liftIO (tryIOError m)

getResponse
  :: (MonadIO m)
  => Handle
  -> [CommandStr]
  -> Parser a
  -> EitherT ClientError m a
getResponse hdl q p = do
  send hdl q
  either (left . ParseError) right =<< (parse p `fmap` recv hdl)

send :: (MonadIO m) => Handle -> [CommandStr] -> EitherT ClientError m ()
send hdl = connIO . SB.hPut hdl . pack

pack :: [CommandStr] -> SB.ByteString
pack = T.encodeUtf8 . T.unlines . f . map render
  where
    f [x] = [x]
    f xs  = ("command_list_ok_begin" : xs) ++ ["command_list_end"]

recv :: (MonadIO m) => Handle -> EitherT ClientError m [SB.ByteString]
recv hdl = go
  where
    go = do
      ln <- connIO (SB.hGetLine hdl)
      if ln == "OK"
        then right []
      else if "ACK" `SB.isPrefixOf` ln
        then left . ProtocolError . T.decodeUtf8 $ SB.drop 4 ln
      else fmap (ln :) go

withConn
  :: (C.MonadMask m, MonadIO m)
  => HostName
  -> PortID
  -> (Handle -> EitherT ClientError m a)
  -> EitherT ClientError m a
withConn host port m = EitherT $
  C.bracket (runEitherT $ open host port) (eh close) (eh m)
  where
    eh f = either (return . Left) (runEitherT . f . fst)

open
  :: (MonadIO m)
  => HostName
  -> PortID
  -> EitherT ClientError m (Handle, SB.ByteString)
open host port = do
  hdl <- connIO (connectTo host port)
  ver <- connIO (SB.hGetLine hdl)
  unless ("OK MPD " `SB.isPrefixOf` ver) $ do
    close hdl
    left InvalidHost
  return (hdl, ver)

close :: (MonadIO m) => Handle -> EitherT ClientError m ()
close hdl = void . liftIO $
  tryIOError (SB.hPut hdl "close\n" >> hClose hdl)

------------------------------------------------------------------------
-- $run  

data ClientError
  = ParseError String
  | ProtocolError T.Text
  | InvalidHost
  | ConnError IOError
  | Custom String
    deriving (Show)

runWith
  :: (C.MonadMask m, MonadIO m)
  => HostName
  -> PortID
  -> Command a
  -> EitherT ClientError m a
runWith host port (Command q p) = withConn host port $ \hdl ->
  getResponse hdl q p

run
  :: (C.MonadMask m, MonadIO m)
  => Command a
  -> EitherT ClientError m a
run = runWith "localhost" (PortNumber 6600)
