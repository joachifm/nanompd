# About

nanompd is a [Haskell](https://haskell.org/) library for scripting client
interactions with [MPD](http://www.musicpd.org/),
the music player daemon.
The library is designed with simplicity in mind and strives to do the least
amount of work to be useful, primarily in service of higher-level client
applications.
The ultimate goal is to abstract away the boring parts of interfacing with MPD
and leave the fun stuff to the client implementor.

# Installation

To install nanompd, do

    (cd nanompd && cabal install)

[Nix](https://nixos.org/nix) users may do

    nix-shell

to obtain a working environment containing everything you'd need to
hack and use nanompd.

# Usage

Interactive use looks like this

    MPD> run ((,) <$> currentSong <*> status)

See the haddocks for details.

# Differences from libmpd-haskell

There are several key differences between
nanompd and [libmpd-haskell](https://hackage.haskell.org/package/libmpd)

- The library is structured around an applicative command interface
  (derived from `Network.MPD.Applicative`).
- Computations against MPD run within an EitherT transformer rather
  than in a custom MPD monad.
- The library core contains minimal client logic (specifically,
  automatic authentication and re-connection has been removed).
- A streamlined API for defining new protocol command wrappers simplifies
  the process of extending the command set.

As a result, nanompd is more compact than libmpd-haskell and its internals
are easier to reason about.

# Contributing

Send patches via email or submit a pull request on
[GitHub](https://github.com/joachifm/nanompd).
Test cases and benchmarks are highly appreciated.
Feel free to append your name to the list of
[CONTRIBUTORS](../blob/master/CONTRIBUTORS) if you deem it appropriate to do
so (anything above fixing a single-letter typo warrants a mention).

# Resources

- `/join #vimus` @ FreeNode
- [The Music Player Daemon protocol](http://www.musicpd.org/doc/protocol/)

# Licence

nanompd is distributed under the MIT licence
(see [COPYING](../blob/master/COPYING)).
