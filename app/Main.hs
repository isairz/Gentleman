{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent.Async
import           Control.Concurrent.MSem
import           Control.Monad
import qualified Data.Text                as T
import qualified Data.Traversable         as T
import           Marumaru
import           Sucuri
import           Text.Show.Unicode
import           Types


main :: IO ()
main = do
  scrapMarumaru
  return ()

mapPool :: T.Traversable t => Int -> (a -> IO b) -> t a -> IO (t b)
mapPool max f xs = do
    sem <- new max
    mapConcurrently (with sem . f) xs

forPool :: T.Traversable t => Int -> t a -> (a -> IO b) -> IO (t b)
forPool max = flip $ mapPool max

scrapMarumaru :: IO [Manga]
scrapMarumaru = do
  mangas <- Marumaru.mangaList
  uprint $ take 10 mangas
  uprint $ length mangas
  mangas' <- forPool 1000 mangas $ \manga -> do
    manga' <- Marumaru.mangaDetail manga
    -- FIXME: Show Progress
    -- uprint $ name manga'
    -- uprint $ (T.unpack $ name manga) ++ " " ++ (ushow $ length $ chapters manga')
    return manga'

  uprint $ length mangas'
  return mangas'
