{-# LANGUAGE OverloadedStrings #-}

module Hitomi
    ( allMangas
    ) where

import           Control.Applicative       (Applicative, (<$>), (<*>))
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy      as L8
import qualified Data.ByteString.Lazy.UTF8 as L8
-- (FromJSON, Value (..), parseJSON, (.:))
import           Network.HTTP.Simple
import           Types                     (Manga (..))

instance FromJSON Manga where
  parseJSON (Object v) = Manga
      <$> v .: "id"
      <*> v .: "n"
      <*> v .:? "a" .!= []
      <*> v .:? "g" .!= []
      <*> v .: "type"
      <*> v .:? "l" .!= ""
      <*> v .:? "p" .!= []
      <*> v .:? "c" .!= []
      <*> v .:? "t" .!= []
  parseJSON _ = mempty
                          -- a .: "authors"
  -- parseJSON _ = mzero

allMangas :: IO [Manga]
allMangas = do
  raw <- liftM getResponseBody (httpLBS $ "https://ltn.hitomi.la/galleries1.json")
  -- raw <- L8.readFile "galleries0.json"
  -- let manga = -- "[{\"id\":123, \"n\":\"asdf\", \"a\":[\"as\",\"gfg\"], \"type\":\"sdf\"}, {\"id\":123, \"n\":\"asdasdfasdff\", \"a\":[\"as\",\"gfg\"], \"type\":\"sdf\"}]" :: String
  case eitherDecode raw of
    Left err -> do
      print err
      return []
    Right mangas -> do
      -- print mangas
      return mangas
