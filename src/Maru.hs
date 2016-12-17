{-# LANGUAGE Arrows                    #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

module Maru
    ( allMangas
    ) where

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy.UTF8  as L8
import           Network.HTTP.Simple
import           Text.HandsomeSoup
import           Text.XML.HXT.Core
import           Types                      (Manga(..))

-- allMangas :: IO (Maybe [Manga])
allMangas :: IO [Manga]
allMangas = do
  response <- httpLBS $ setRequestHeader "User-Agent" ["Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_4) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/50.0.2661.94 Safari/537.36"]
                      $ "http://marumaru.in/c/1"
  let doc = parseHtml . L8.toString . getResponseBody $ response
  mangas <- runX $ doc >>> getManga
  return mangas

getManga :: ArrowXml a => a XmlTree Manga
getManga = css ".widget_review01 ul li"
           >>> proc x -> do
                 title <- (css "div" //> getText) -< x
                 link <- (css "a" ! "href") -< x
                 returnA -< Manga
                  { title = title
                  , link = link
                  }
