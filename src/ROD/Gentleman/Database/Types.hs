{-# LANGUAGE OverloadedStrings #-}

module ROD.Gentleman.Database.Types where

import Data.Text

newtype Key entity =
  Key Text
  deriving (Eq, Ord)

instance Show (Key entity) where
  show (Key k) = show k

type MangaId = Key Manga

data Manga = Manga
  { mangaName :: Text
  , mangaAuthors :: [Text]
  , mangaGroups :: [Text]
  , mangaType :: Text
  , mangaLanguage :: Text
  , mangaSerieses :: [Text]
  , mangaCharacters :: [Text]
  , mangaTags :: [Text]
  }

type ChapterId = Key Chapter

data Chapter = Chapter
  { chapterName :: Text
  , chapterSrcs :: [Text]
  } deriving (Show, Eq)

defaultManga =
  Manga
  { mangaName = ""
  , mangaAuthors = emptyArray
  , mangaGroups = emptyArray
  , mangaType = ""
  , mangaLanguage = ""
  , mangaSerieses = emptyArray
  , mangaCharacters = emptyArray
  , mangaTags = emptyArray
  }
  where
    emptyArray = []
