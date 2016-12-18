{-# LANGUAGE Arrows            #-}
{-# LANGUAGE OverloadedStrings #-}

module Marumaru
    ( mangaList
    , mangaDetail
    ) where

import           Control.Monad
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Lazy.UTF8  as BL
import           Data.Char                  (isDigit)
import           Data.Maybe
import qualified Data.Text                  as T
import           Network.HTTP.Simple
import           Text.HTML.DOM              as Html
import           Text.Show.Unicode
import           Text.XML
import           Text.XML.Cursor
import           Types                      (Chapter (..), Manga (..),
                                             defaultManga)

lastInteger :: Read a => T.Text -> a
lastInteger t = read . T.unpack $ T.takeWhileEnd isDigit t

mangaList :: IO [Manga]
mangaList = do
  response <- httpLBS $ setRequestHeader "User-Agent" ["Mozilla/5.0 (Linux; Android 5.0; SM-G900P Build/LRX21T) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/48.0.2564.23 Mobile Safari/537.36"]
                      $ "http://marumaru.in/p/mobilemangamain"
  let cursor = fromDocument . Html.parseLBS . getResponseBody $ response
  return $ cursor $// attributeIs "class" "widget_review01"
                  &/  element "ul" &/ element "li"
                  >=> \x -> do
                       let title = head $ x $// element "div" &// content
                       let link = head $ x $// element "a" >=> attribute "href"
                       return defaultManga
                             { Types.id   = lastInteger link
                             , name       = title
                             }

mangaDetail :: Manga -> IO Manga
mangaDetail manga = do
  url <- parseRequest ("http://marumaru.in/b/manga/" ++ (show $ Types.id manga))
  response <- httpLBS $ setRequestHeader "User-Agent" ["Mozilla/5.0 (Linux; Android 5.0; SM-G900P Build/LRX21T) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/48.0.2564.23 Mobile Safari/537.36"]
                      $ url
  let cursor = fromDocument . Html.parseLBS . getResponseBody $ response
  let links = catMaybes $ cursor
                  $// attributeIs "id" "vContent" &// element "a"
                  >=> \x -> do
                       let name = T.concat $ x $// content
                       let link = head $ x $| attribute "href"
                       return $ if ((not . T.null $ name) && ("archives" `T.isInfixOf` link))
                                then Just Chapter
                                    { chapter_id = lastInteger link
                                    , chapter_name = name
                                    }
                                else Nothing

  return manga { chapters = links }
