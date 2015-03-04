{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Gen where

import qualified Data.ByteString.Char8 as SB

import Test.QuickCheck

songInfoG = do
  fileName <- pathG
  lastModified <- dateG
  time <- digits 4

  playing <- arbitrary
  pos <- if playing
         then Just <$> digits 4
         else return Nothing
  id_ <- if playing
         then Just <$> digits 4
         else return Nothing

  tags <- tagsG

  return $ [ "file: " <> fileName
           , "Last-Modified: " <> lastModified
           , "Time: " <> time
           ]
           ++ maybe [] (\x -> ["Pos: " <> x]) pos
           ++ maybe [] (\x -> ["Id: " <> x]) id_
           ++ tags

tagsG = listOf $ do
  (k, g) <- elements tag
  v <- g
  return (k <> ": " <> v)
  where
    tag  = [ ("Artist",      textG)
           , ("AlbumArtist", textG)
           , ("Title",       textG)
           , ("Album",       textG)
           , ("Track", do
                 a <- digits 2
                 b <- digits 2
                 return (a <> "/" <> b))
           , ("Date", digits 4)
           , ("Genre", textG)
           ]

-- XXX: must quote path separators in names
pathG = do
  name <- SB.intercalate "/" <$> listOf1 textG
  isFile <- arbitrary
  if not isFile then return name else do
    ext <- elements [ "mp3", "ogg", "flac" ]
    return (name <> "." <> ext)

dateG = do
  (year, month, day) <- (,,) <$> digits 4 <*> digits 2 <*> digits 2
  (hours, mins, secs) <- (,,) <$> digits 2 <*> digits 2 <*> digits 2
  return (mconcat [ year, "-", month, "-", day, "T"
                  , hours, ":", mins, ":", secs, "Z" ])

textG = fromString <$> listOf1 (elements chars)
  where chars = [ 'a'..'z' ] ++ [ 'A' .. 'Z' ] ++ ".,:;+-*/!@#$%&{}[]()?`\\"

digits n = fromString <$> vectorOf n (elements ['0'..'9'])

fromShow :: (Show a, IsString s) => a -> s
fromShow = fromString . show
