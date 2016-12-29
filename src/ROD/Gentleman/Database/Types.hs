{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module ROD.Gentleman.Database.Types
    ( Manga(..)
    , defaultManga
    , Chapter(..)
    , Page(..)
    ) where

import           Control.Monad.IO.Class      (liftIO)
import           Data.Text
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Manga
  idx        Int
  name       Text
  authors    [Text]
  groups     [Text]
  type       Text
  language   Text
  serieses   [Text]
  characters [Text]
  tags       [Text]
  Primary    idx
  deriving   Show

Chapter
  idx      Int
  name     Text
  deriving Show

Page
  src Text
|]

defaultManga = Manga
  { mangaIdx         = 0
  , mangaName       = ""
  , mangaAuthors    = []
  , mangaGroups     = []
  , mangaType       = ""
  , mangaLanguage   = ""
  , mangaSerieses   = []
  , mangaCharacters = []
  , mangaTags       = []
  }
