{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module ROD.Gentleman.Database.Store
    ( module Database.Persist
    , module Database.Persist.Postgresql
    ) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Control  (MonadBaseControl)
import           Data.Maybe                   (fromMaybe)
import           Data.Monoid                  ((<>))

import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH

import           ROD.Gentleman.Database.Types

-- runConn :: (MonadIO m, MonadBaseControl IO m) => SqlPersistT m t -> m ()
-- runConn f = do
--     withPostgresqlPool ("host=" <> "localhost" <> " port=5432 user=postgres dbname=test") 4 $ runSqlPool f
--     return ()
--
-- saveManga :: Manga -> ()
-- saveManga = insert >> return ()
