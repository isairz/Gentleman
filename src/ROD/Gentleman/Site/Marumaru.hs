{-# LANGUAGE OverloadedStrings #-}

module ROD.Gentleman.Site.Marumaru
    ( mangaList
    , mangaDetail
    , imageList
    , getCookieJar
    ) where

import           Control.Monad
import           Control.Retry
import           Data.Char                     (isDigit, ord)
import           Data.Maybe                    (catMaybes)
import           Data.Monoid                   ((<>))

import qualified Data.ByteString.Char8         as B
import qualified Data.ByteString.Lazy.Char8    as BL
import qualified Data.ByteString.Lazy.Search   as BL
import qualified Data.ByteString.Lazy.UTF8     as BLcx
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T

import           Network.HTTP.Conduit          hiding (httpLBS)
import           Network.HTTP.Simple           hiding (httpLbs)
import           Text.HTML.DOM                 (parseLBS)
import           Text.Show.Unicode
import           Text.XML                      hiding (parseLBS)
import           Text.XML.Cursor

import           ROD.Gentleman.Site.CloudFlare (decryptCookie)
import           ROD.Gentleman.Types           (Chapter (..), Manga (..),
                                                Page (..), defaultManga)


lastToInt :: T.Text -> Int
lastToInt t = T.foldl (\n c -> n * 10 + ord c - ord '0') 0
                        $ T.takeWhileEnd isDigit t

limitedBackoff = exponentialBackoff 50 <> limitRetries 10

requestWithRetry :: Request -> IO (Response BL.ByteString)
requestWithRetry req = recoverAll limitedBackoff (\re -> do
  if rsIterNumber re > 0
    then putStrLn $ B.unpack (host req) ++ B.unpack (path req) ++ " " ++ show (rsIterNumber re)
    else pure ()
  httpLBS req
  )

requestDoc :: String -> IO Document
requestDoc url = do
  url' <- parseRequest url
  response <- requestWithRetry $ setRequestHeader "User-Agent" ["Mozilla/5.0 (Linux; Android 5.0; SM-G900P Build/LRX21T) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/48.0.2564.23 Mobile Safari/537.36"] url'
  return . parseLBS . getResponseBody $ response

mangaList :: IO [Manga]
mangaList = do
  doc <- requestDoc "http://marumaru.in/p/mobilemangamain"
  return $ fromDocument doc
    $// attributeIs "class" "widget_review01" &/  element "ul" &/ element "li"
    >=> \x -> do
         let title = head $ x $// element "div" &// content
         let link = head $ x $// element "a" >=> attribute "href"
         return defaultManga
               { mangaIdx        = lastToInt link
               , mangaName       = title
               }

mangaDetail :: Manga -> IO (Manga, [Chapter])
mangaDetail manga = do
  doc <- requestDoc ("http://marumaru.in/b/manga/" ++ show (mangaIdx manga))
  let links = catMaybes $ fromDocument doc
                            $// attributeIs "id" "vContent" &// element "a"
                            >=> \x -> do
                                 let name = T.concat $ x $// content
                                 let link = head $ x $| attribute "href"
                                 return $ if (not . T.null $ name) && ("archives" `T.isInfixOf` link)
                                          then Just Chapter
                                              { chapterIdx = lastToInt link
                                              , chapterName = name
                                              }
                                          else Nothing
  -- Error Log for no parsing
  -- if null links
  --   then BL.putStrLn $ renderLBS def doc
  --   else pure ()
  return (manga, links)

imageList :: CookieJar -> Int -> IO [Page]
imageList jar i = do
  req' <- parseRequest ("http://www.yuncomics.com/archives/" ++ show i ++ "?549234")
  let req = setRequestHeader "User-Agent" ["Android 5.0"]
          $ setRequestHeader "Cache-Control" ["max-age=0"]
          $ setRequestHeader "Upgrade-Insecure-Requests" ["1"]
          $ req' { cookieJar = Just jar }
  body <- getResponseBody <$> requestWithRetry req
  let srcs = getDataSrcs body
  -- if null srcs
  --   then BL.putStrLn body
  --   else pure ()
  return srcs

getDataSrcs :: BL.ByteString -> [Page]
getDataSrcs str = do
  let (src, left) = BL.breakOn "\"" . snd $ BL.breakAfter "data-src=\"" str
  if BL.null left then [] else Page (T.decodeUtf8 (BL.toStrict src)) : getDataSrcs left


getCookieJar :: IO CookieJar
getCookieJar = do
  -- cloudflare cookie
  manager <- newManager tlsManagerSettings
  req' <- parseRequest "http://www.yuncomics.com/archives/213688?123124"
  let req1 = setRequestHeader "User-Agent" ["Android 5.0"]
           $ setRequestHeader "Cache-Control" ["no-cache"] req'
  res1 <- httpLbs req1 manager
  let cookie = decryptCookie $ getResponseBody res1
  let jar1 = createCookieJar $ cookie:destroyCookieJar (responseCookieJar res1)

  -- login
  req' <- parseRequest "POST http://www.yuncomics.com/wp-login.php?action=postpass"
  let req2 = setRequestBodyURLEncoded[("post_password", "qndxkr"), ("Submit", "Submit")]
           $ setRequestHeader "User-Agent" ["Android 5.0"]
           -- $ setRequestHeader "Cache-Control" ["no-cache"]
           $ req' { cookieJar = Just jar1 }
  res2 <- httpLbs req2 manager
  let jar2 = responseCookieJar res2

  return jar2
