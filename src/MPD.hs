{-# LANGUAGE OverloadedStrings #-}

module MPD
  (
    Command
  , run
  , runWith

  , ping
  , status
  , currentsong
  , listallinfo
  ) where

import MPD.Core
import Data.ByteString (ByteString)

------------------------------------------------------------------------

ping :: Command ()
ping = Command ["ping"] (return ())

currentsong :: Command [ByteString]
currentsong = Command ["currentsong"] id_

status :: Command [ByteString]
status = Command ["status"] id_

listallinfo :: Command [ByteString]
listallinfo = Command ["listallinfo"] id_
