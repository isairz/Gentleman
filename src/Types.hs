{-# LANGUAGE OverloadedStrings #-}

module Types
    ( Manga(..)
    -- , mangaDefault
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
  } deriving (Show, Eq)

-- data Chapter = Chapter
--   { id   :: Int
--   , name :: Text
--   }

-- pages

-- mangaDefault = Manga
--   { name = ""
--   , authors    = []
--   , groups     = []
--   , type'      = ""
--   , language   = ""
--   , serieses   = []
--   , characters = []
--   , tags       = []
--   }
