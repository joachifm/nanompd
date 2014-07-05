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
import MPD.Util

import Control.DeepSeq (NFData(..), deepseq)

import qualified Data.List           as L
import qualified Data.HashMap.Strict as M

import Data.ByteString (ByteString)
import Data.Text (Text)

------------------------------------------------------------------------
-- MPD protocol command API.

ping :: Command ()
ping = Command ["ping"] (return ())

currentsong :: Command SongInfo
currentsong = Command ["currentsong"] (liftFold songInfo)

status :: Command [ByteString]
status = Command ["status"] id_

listallinfo :: Command [ByteString]
listallinfo = Command ["listallinfo"] id_

------------------------------------------------------------------------
-- MPD protocol objects.

data SongInfo = SongInfo
  { songFile :: {-# UNPACK #-} !Text
  , songId   :: {-# UNPACK #-} !Text
  , songPos  :: {-# UNPACK #-} !Text
  , songTags :: !(M.HashMap ByteString Text)
  } deriving (Show)

instance NFData SongInfo where
  rnf x = songFile x `deepseq`
          songId x   `deepseq`
          songPos x  `deepseq`
          songTags x `deepseq` ()

songInfo :: [ByteString] -> SongInfo
songInfo = L.foldl' step initial
  where
    step z x = case pair x of
      ("file", v) -> z { songFile = v }
      ("Id", v)   -> z { songId = v }
      ("Pos", v)  -> z { songPos = v }
      (k, v)      -> z { songTags = M.insert k v (songTags z) }

    initial = SongInfo
      { songFile = ""
      , songId = ""
      , songPos = ""
      , songTags = M.empty
      }
