{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module ROD.Gentleman.Database.Store where

import ROD.Gentleman.Database.Types
-- runConn :: (MonadIO m, MonadBaseControl IO m) => SqlPersistT m t -> m ()
-- runConn f = do
--     withPostgresqlPool ("host=" <> "localhost" <> " port=5432 user=postgres dbname=test") 4 $ runSqlPool f
--     return ()
--
-- saveManga :: Manga -> ()
-- saveManga = insert >> return ()
