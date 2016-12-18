{-# LANGUAGE Arrows            #-}
{-# LANGUAGE OverloadedStrings #-}

module Marumaru
    ( allMangas
    ) where

import           Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Lazy.UTF8  as BL
import qualified Data.Text                  as T
import           Network.HTTP.Simple
import           Text.HandsomeSoup
import           Text.XML.HXT.Core
import           Types                      (Manga (..))

allMangas :: IO [Manga]
allMangas = do
  response <- httpLBS $ setRequestHeader "User-Agent" ["Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_4) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/50.0.2661.94 Safari/537.36"]
                      $ "http://marumaru.in/c/1"
  let doc = parseHtml . BL.toString . getResponseBody $ response
  mangas <- runX $ doc >>> getManga
  return mangas

getManga :: ArrowXml a => a XmlTree Manga
getManga = css ".widget_review01 ul li"
           >>> proc x -> do
                 title <- (css "div" //> getText) -< x
                 link <- (css "a" ! "href") -< x
                 returnA -< Manga -- mangaDefault
                  { Types.id   = read . dropWhile (not . liftM2 (&&) (>='0') (<='9')) $ link
                  , name       = T.pack title
                  , authors    = []
                  , groups     = []
                  , type'      = ""
                  , language   = ""
                  , serieses   = []
                  , characters = []
                  , tags       = []
                  }
