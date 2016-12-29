{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent.Async
import           Control.Concurrent.MSem
import           Control.Monad
import           Data.List
import qualified Data.Text                   as T
import qualified Data.Text.IO                as T
import qualified Data.Traversable            as T

import           ROD.Gentleman.Site.Marumaru as Marumaru
import           ROD.Gentleman.Types         as Types


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
    putStrLn $ "[" ++ show idx ++ "/" ++ show (length mangas) ++ "] "
      ++ T.unpack (name manga) ++ " " ++ show (length $ chapters manga')
      ++ "\n\t" ++ intercalate "\n\t" (map (\c -> T.unpack (chapter_name c) ++ " " ++ show (chapter_id c)) $ chapters manga')
    return manga'

  -- Get Image srcs
  let cids = map chapter_id $ concat $ chapters <$> mangas'
  print $ show $ length cids
  jar <- Marumaru.getCookieJar
  forPool 1000 cids $ \cid -> do
    srcs <- Marumaru.imageList jar cid
    putStrLn $ show cid ++ " (" ++ show (length srcs) ++ ")"
  return mangas'

singleMarumaru cid = do
  jar <- Marumaru.getCookieJar
  srcs <- Marumaru.imageList jar cid
  mapM_ T.putStrLn srcs
  return ()
