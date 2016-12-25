{-# LANGUAGE OverloadedStrings #-}

module ROD.Gentleman.Site.Hitomi
    ( allMangas
    ) where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Network.HTTP.Simple
import           ROD.Gentleman.Types (Manga (..))

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

allMangas :: IO [Manga]
allMangas = do
  raw <- liftM getResponseBody (httpLBS $ "https://ltn.hitomi.la/galleries0.json")
  case eitherDecode raw of
    Left err -> do
      print err
      return []
    Right mangas -> do
      -- print mangas
      return mangas
