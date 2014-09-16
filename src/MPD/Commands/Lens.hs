module MPD.Commands.Lens
  ( songFileL
  , songLastModifiedL
  , songTimeL
  , songTagsL
  , songPosL
  , songIdL
  , viewTagL
  ) where

import MPD.Commands.Types
import MPD.Core (Label)

import Control.Applicative (Applicative(..))
import Data.Text
import Data.Time.Clock (UTCTime)
import qualified Data.HashMap.Strict as M

_Path :: (Functor f) => (Text -> f Text) -> Path -> f Path
_Path f s = fmap (\a -> Path a) (f (unPath s))

songFileL :: (Functor f) => (Path -> f Path) -> SongInfo -> f SongInfo
songFileL f s = fmap (\a -> s { songFile = a }) (f (songFile s))

songLastModifiedL
  :: (Functor f)
  => (UTCTime -> f UTCTime)
  -> SongInfo
  -> f SongInfo
songLastModifiedL f s =
  fmap (\a -> s { songLastModified = a }) (f (songLastModified s))

songTimeL
  :: (Functor f)
  => (Seconds -> f Seconds)
  -> SongInfo
  -> f SongInfo
songTimeL f s = fmap (\a -> s { songTime = a }) (f (songTime s))

songTagsL
  :: (Functor f)
  => (M.HashMap Label Text
  -> f (M.HashMap Label Text))
  -> SongInfo
  -> f SongInfo
songTagsL f s = fmap (\a -> s { songTags = a }) (f (songTags s))

songPosL
  :: (Functor f)
  => (Maybe SongPos -> f (Maybe SongPos))
  -> SongInfo
  -> f SongInfo
songPosL f s = fmap (\a -> s { songPos = a }) (f (songPos s))

songIdL
  :: (Functor f)
  => (Maybe SongId -> f (Maybe SongId))
  -> SongInfo
  -> f SongInfo
songIdL f s = fmap (\a -> s { songId = a }) (f (songId s))

viewTagL :: (Functor f) => Label -> (Maybe Text -> f (Maybe Text)) -> SongInfo -> f SongInfo
viewTagL lbl f s = fmap (\a -> case a of Just a' -> s { songTags = M.insert lbl a' (songTags s)  }
                                         _ -> s)
                        (f (M.lookup lbl (songTags s)))

