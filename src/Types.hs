{-# LANGUAGE OverloadedStrings #-}

module Types
    ( Manga(..)
    , Chapter(..)
    , Page(..)
    , defaultManga
    ) where

import           Data.Text

data Manga = Manga
  { id         :: Int
  , name       :: Text
  , authors    :: [Text]
  , groups     :: [Text]
  , type'      :: Text
  , language   :: Text
  , serieses   :: [Text]
  , characters :: [Text]
  , tags       :: [Text]
  , chapters   :: [Chapter]
  } deriving (Show, Eq)

data Chapter = Chapter
  { chapter_id   :: Int
  , chapter_name :: Text
  } deriving (Show, Eq)

type Page = Text

defaultManga = Manga
  { Types.id   = 0
  , name       = ""
  , authors    = []
  , groups     = []
  , type'      = ""
  , language   = ""
  , serieses   = []
  , characters = []
  , tags       = []
  , chapters   = []
  }
