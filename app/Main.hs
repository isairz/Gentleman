{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}


module Main where

import           Control.Concurrent.Async
import           Control.Concurrent.MSem
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger         (runStdoutLoggingT)
import           Control.Monad.Trans.Control  (MonadBaseControl)
import           Control.Monad.Trans.Resource (ResourceT, runResourceT)
import           Data.List
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as T
import qualified Data.Traversable             as T

import           ROD.Gentleman.Database.Store as Store
import           ROD.Gentleman.Database.Types
import           ROD.Gentleman.Site.Marumaru  as Marumaru


main :: IO ()
main = do
  -- mangas <- scrapMarumaru
  -- singleMarumaru 1195171
  saveMarumaru
  return ()

mapPool :: T.Traversable t => Int -> (a -> IO b) -> t a -> IO (t b)
mapPool max f xs = do
    sem <- new max
    mapConcurrently (with sem . f) xs

forPool :: T.Traversable t => Int -> t a -> (a -> IO b) -> IO (t b)
forPool max = flip $ mapPool max

scrapMarumaru :: IO [(Manga, [Chapter])]
scrapMarumaru = do
  -- Get Manga List
  mangas <- Marumaru.mangaList

  -- Get Manga Detail & Chapter Link
  mangaDetails <- forPool 1000 (zip mangas [1..]) $ \(manga, idx) -> do
    (manga', chapters) <- Marumaru.mangaDetail $ mangaIdx manga
    -- FIXME: Show Progress
    -- putStrLn $ "[" ++ show idx ++ "/" ++ show (length mangas) ++ "] "
    --   ++ T.unpack (mangaName manga) ++ " " ++ show (length chapters)
    --   ++ "\n\t" ++ intercalate "\n\t" (map (\c -> T.unpack (chapterName c) ++ " " ++ show (chapterIdx c)) chapters)
    return (manga', chapters)

  -- Get Image srcs
  -- let cids = map chapterIdx $ concat $ snd <$> mangaDetails
  -- print $ show $ length cids
  -- jar <- Marumaru.getCookieJar
  -- forPool 1000 cids $ \cid -> do
  --   srcs <- Marumaru.imageList jar cid
  --   putStrLn $ show cid ++ " (" ++ show (length srcs) ++ ")"
  return mangaDetails

singleMarumaru cid = do
  jar <- Marumaru.getCookieJar
  pages <- Marumaru.imageList jar cid
  mapM_ (T.putStrLn . pageSrc) pages
  return ()

connstr :: ConnectionString
connstr = "host=" <> "localhost" <> " port=5432 user=isair dbname=knowledge"

saveMarumaru :: IO ()
saveMarumaru = runStdoutLoggingT $ withPostgresqlPool connstr 16 $ \pool -> liftIO $ do
  runSqlPersistMPool (runMigration migrateAll) pool
  mangas <- Marumaru.mangaList
  print $ length mangas
  -- Get Manga Detail & Chapter Link
  mangaDetails <- forPool 1000 (zip mangas [1..]) $ \(manga, idx) -> do
    (manga', chapters) <- Marumaru.mangaDetail $ mangaIdx manga
    -- Show Progress
    putStrLn $ "[" ++ show idx ++ "/" ++ show (length mangas) ++ "] "
      ++ T.unpack (mangaName manga) ++ " " ++ show (length chapters)
      ++ "\n\t" ++ intercalate "\n\t" (map (\c -> T.unpack (chapterName c) ++ " " ++ show (chapterIdx c)) chapters)
    return (manga', chapters)
  runSqlPool (mapM (\(m, _) -> repsert (toSqlKey $ fromIntegral $ mangaIdx m) m) mangaDetails) pool
  return ()
