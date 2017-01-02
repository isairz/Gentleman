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
    ( module ROD.Gentleman.Database.Types
    , module Database.Persist.Class
    , module Database.Persist.Postgresql
    , module Database.Persist.Postgresql.Json
    ) where

import           Control.Monad.IO.Class           (liftIO)
import           Data.Aeson
import           Data.Maybe
import           Data.Text
import           Database.Persist
import           Database.Persist.Class
import           Database.Persist.Postgresql
import           Database.Persist.Postgresql.Json
import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Manga
  idx        Int
  name       Text
  authors    Jsonb
  groups     Jsonb
  type       Text
  language   Text
  serieses   Jsonb
  characters Jsonb
  tags       Jsonb
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
  , mangaAuthors    = emptyArray
  , mangaGroups     = emptyArray
  , mangaType       = ""
  , mangaLanguage   = ""
  , mangaSerieses   = emptyArray
  , mangaCharacters = emptyArray
  , mangaTags       = emptyArray
  } where emptyArray = Jsonb $ toJSON ()
