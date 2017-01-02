{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Concurrent.Async
import Control.Concurrent.MSem
import Data.List
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Traversable

import ROD.Gentleman.Database.Store as Store
import ROD.Gentleman.Database.Types
import ROD.Gentleman.Site.Marumaru as Marumaru

main :: IO ()
main = do
  mangas <- scrapMarumaru
  -- singleMarumaru $ Key 1814648
  -- saveMarumaru
  return ()

mapPool
  :: Traversable t
  => Int -> (a -> IO b) -> t a -> IO (t b)
mapPool max f xs = do
  sem <- new max
  mapConcurrently (with sem . f) xs

forPool
  :: Traversable t
  => Int -> t a -> (a -> IO b) -> IO (t b)
forPool max = flip $ mapPool max

-- scrapMarumaru :: IO [(Manga, [Chapter])]
scrapMarumaru = do
  mids <- Marumaru.mangaList
  print $ length mids
  -- Get Manga Detail & Chapter Link
  mangaDetails <-
    forPool 1000 (zip mids [1 ..]) $ \(mid, idx) -> do
      (manga, cids) <- Marumaru.mangaDetail mid
    -- FIXME: Show Progress
      putStrLn $
        ("[" ++ show idx ++ "/" ++ show (length mids) ++ "] ") ++
        (T.unpack (mangaName manga) ++ " " ++ show (length cids)) ++
        ("\n\t" ++ intercalate ", " (map show cids))
      return (manga, cids)
  -- Get Image srcs
  let mangas = fst <$> mangaDetails
  let cids = sort $ concat $ snd <$> mangaDetails
  print $ show $ length cids
  jar <- Marumaru.getCookieJar
  chapters <-
    forPool 1000 cids $ \cid -> do
      chapter <- Marumaru.chapterDetail jar cid
      case chapter of
        Just c ->
          putStrLn $
          (show cid ++ " " ++ T.unpack (chapterName c)) ++
          " (" ++ show (length $ chapterSrcs c) ++ ")"
        Nothing -> putStrLn (show cid ++ "!!!!!")
      return chapter
  return mangaDetails

singleMarumaru cid = do
  jar <- Marumaru.getCookieJar
  pages <- Marumaru.chapterDetail jar cid
  print pages
  -- mapM_ T.putStrLn $ chapterSrcs pages
  return ()
-- connstr :: ConnectionString
-- connstr = "host=" <> "localhost" <> " port=5432 user=isair dbname=knowledge"
-- saveMarumaru :: IO ()
-- saveMarumaru =
--   runStdoutLoggingT $
--   withPostgresqlPool connstr 16 $ \pool ->
--     liftIO $ do
--       runSqlPersistMPool (runMigration migrateAll) pool
--       mangas <- Marumaru.mangaList
--       print $ length mangas
--   -- Get Manga Detail & Chapter Link
--       mangaDetails <-
--         forPool 1000 (zip mangas [1 ..]) $ \(manga, idx) -> do
--           (manga', chapters) <- Marumaru.mangaDetail $ mangaIdx manga
--     -- Show Progress
--           putStrLn $
--             "[" ++
--             show idx ++
--             "/" ++
--             show (length mangas) ++
--             "] " ++
--             T.unpack (mangaName manga) ++
--             " " ++
--             show (length chapters) ++
--             "\n\t" ++
--             intercalate
--               "\n\t"
--               (map
--                  (\c -> T.unpack (chapterName c) ++ " " ++ show (chapterIdx c))
--                  chapters)
--           return (manga', chapters)
--       runSqlPool
--         (mapM
--            (\(m, _) -> repsert (toSqlKey $ fromIntegral $ mangaIdx m) m)
--            mangaDetails)
--         pool
--       return ()
