{-# LANGUAGE OverloadedStrings #-}

module Marumaru
    ( mangaList
    , mangaDetail
    ) where

import           Control.Monad
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Lazy.UTF8  as BL
import           Data.Char                  (isDigit, ord)
import           Data.Maybe
import qualified Data.Text                  as T
import           Network.HTTP.Simple
import           Text.HTML.DOM              as Html
import           Text.Show.Unicode
import           Text.XML
import           Text.XML.Cursor
import           Types                      (Chapter (..), Manga (..),
                                             defaultManga)

lastToInt :: T.Text -> Int
lastToInt t = T.foldl (\n c -> n * 10 + ord c - ord '0') 0
                        $ T.takeWhileEnd isDigit t

requestDoc :: String -> IO Document
requestDoc url = do
  url' <- parseRequest url
  response <- httpLBS $ setRequestHeader "User-Agent" ["Mozilla/5.0 (Linux; Android 5.0; SM-G900P Build/LRX21T) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/48.0.2564.23 Mobile Safari/537.36"]
                        url'
  return . Html.parseLBS . getResponseBody $ response

mangaList :: IO [Manga]
mangaList = do
  doc <- requestDoc "http://marumaru.in/p/mobilemangamain"
  return $ fromDocument doc
    $// attributeIs "class" "widget_review01" &/  element "ul" &/ element "li"
    >=> \x -> do
         let title = head $ x $// element "div" &// content
         let link = head $ x $// element "a" >=> attribute "href"
         return defaultManga
               { Types.id   = lastToInt link
               , name       = title
               }

mangaDetail :: Manga -> IO Manga
mangaDetail manga = do
  doc <- requestDoc ("http://marumaru.in/b/manga/" ++ show (Types.id manga))
  let links = catMaybes $ fromDocument doc
                            $// attributeIs "id" "vContent" &// element "a"
                            >=> \x -> do
                                 let name = T.concat $ x $// content
                                 let link = head $ x $| attribute "href"
                                 return $ if (not . T.null $ name) && ("archives" `T.isInfixOf` link)
                                          then Just Chapter
                                              { chapter_id = lastToInt link
                                              , chapter_name = name
                                              }
                                          else Nothing
  return manga { chapters = links }
