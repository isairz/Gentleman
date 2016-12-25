{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent.Async
import           Control.Concurrent.MSem
import           Control.Monad
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import qualified Data.Traversable         as T
import           Marumaru
import           Sucuri
import           Text.Show.Unicode
import           Types


main :: IO ()
main = do
  mangas <- scrapMarumaru
  -- singleMarumaru 1195171
  return ()

mapPool :: T.Traversable t => Int -> (a -> IO b) -> t a -> IO (t b)
mapPool max f xs = do
    sem <- new max
    mapConcurrently (with sem . f) xs

forPool :: T.Traversable t => Int -> t a -> (a -> IO b) -> IO (t b)
forPool max = flip $ mapPool max

scrapMarumaru :: IO [Manga]
scrapMarumaru = do
  -- Get Manga List
  mangas <- Marumaru.mangaList

  -- Get Manga Detail & Chapter Link
  mangas' <- forPool 1000 (zip mangas [1..]) $ \(manga, idx) -> do
    manga' <- Marumaru.mangaDetail manga
    -- FIXME: Show Progress
    T.putStr $ T.pack ("[" ++ show idx ++ "/" ++ show (length mangas) ++ "] ")
      `T.append` name manga `T.append` " " `T.append` T.pack (show $ length $ chapters manga') `T.append` "\n"
    return manga'

  -- mapM_ (mapM_ (print . chapter_id) . chapters) mangas'
  return []
  -- Get Image srcs
  -- let cids = map chapter_id $ concat $ chapters <$> mangas'
  -- jar <- Marumaru.getCookieJar
  -- forPool 1000 cids $ \cid -> do
  --   srcs <- Marumaru.imageList jar cid
  --   T.putStrLn $ T.pack (show cid) `T.append` " (" `T.append` T.pack (show $ length srcs) `T.append` ")"
  --   -- mapM_ T.putStrLn srcs
  -- return mangas'

singleMarumaru cid = do
  jar <- Marumaru.getCookieJar
  srcs <- Marumaru.imageList jar cid
  mapM_ T.putStrLn srcs
  return ()
