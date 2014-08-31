{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE Safe #-}

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

    -- * Client errors
    module MPD.Core.ClientError

    -- * Defining MPD protocol command wrappers
  , module MPD.Core.Command
  , module MPD.Core.CommandArg
  , module MPD.Core.CommandStr
  , module MPD.Core.Parser

    -- * Executing MPD protocol command wrappers
  , module MPD.Core.Run

    -- * Connection primitives
  , module MPD.Core.Conn
  ) where

import MPD.Core.ClientError
import MPD.Core.Command
import MPD.Core.CommandArg
import MPD.Core.CommandStr
import MPD.Core.Conn
import MPD.Core.Parser
import MPD.Core.Run

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
record structure using 'field_', as in

@
fooParser = (,) <\$\> field_ "key" valueParser <\*\> field_ "key" valueParser
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

To summarise, adding a new command wrapper follows these steps

* consult the MPD protocol specification and note the command syntax;
* if necessary, add argument types and 'CommandArg' instances;
* if necessary, add protocol object types and parsers; and
* implement the command wrapper using 'CommandStr' to specify
  the protocol command string and define a parser for the response.
-}
