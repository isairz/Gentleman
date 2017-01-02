{-# LANGUAGE OverloadedStrings #-}

module ROD.Gentleman.Site.Marumaru
  ( mangaList
  , mangaDetail
  , chapterDetail
  , getCookieJar
  ) where

import Control.Monad
import Control.Retry
import Data.Char (isDigit, ord)
import Data.Function
import Data.List
import Data.Maybe
import Data.Monoid ((<>))

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Lazy.Search as BL
import qualified Data.ByteString.Lazy.UTF8 as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Network.HTTP.Conduit hiding (httpLBS)
import Network.HTTP.Simple hiding (httpLbs)
import Text.HTML.DOM (parseLBS)
import Text.Show.Unicode
import Text.XML hiding (parseLBS)
import Text.XML.Cursor

import ROD.Gentleman.Database.Types
import ROD.Gentleman.Site.CloudFlare (decryptCookie)

lastToInt :: T.Text -> Int
lastToInt t =
  T.foldl (\n c -> n * 10 + ord c - ord '0') 0 $ T.takeWhileEnd isDigit t

limitedBackoff = exponentialBackoff 50 <> limitRetries 5

requestWithRetry :: Request -> IO (Response BL.ByteString)
requestWithRetry req =
  recoverAll
    limitedBackoff
    (\re -> do
       if rsIterNumber re > 0
         then putStrLn $
              B.unpack (host req) ++
              B.unpack (path req) ++ " " ++ show (rsIterNumber re)
         else pure ()
       httpLBS req)

requestDoc :: String -> IO Document
requestDoc url = do
  url' <- parseRequest url
  response <-
    requestWithRetry $
    setRequestHeader
      "User-Agent"
      [ "Mozilla/5.0 (Linux; Android 5.0; SM-G900P Build/LRX21T) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/48.0.2564.23 Mobile Safari/537.36"
      ]
      url'
  return . parseLBS . getResponseBody $ response

mangaList :: IO [MangaId]
mangaList = do
  doc <- requestDoc "http://marumaru.in/p/mobilemangamain"
  return $
    sort $
    fmap (Key . lastToInt) $
    fromDocument doc $// attributeIs "class" "widget_review01" &/ element "ul" &/
    element "li" &//
    element "a" >=>
    attribute "href"
      -- let title = T.concat $ x $// element "div" &// content
      -- let idx = lastToInt $ T.concat $ x $// element "a" >=> attribute "href"
      -- return defaultManga {mangaIdx = idx, mangaName = title}

mangaDetail :: MangaId -> IO (Manga, [ChapterId])
mangaDetail mid = do
  doc <- requestDoc ("http://marumaru.in/b/manga/" ++ show mid)
  let rawTag =
        T.concat $
        fromDocument doc $// element "meta" >=>
        attributeIs "name" "keywords" >=> attribute "content"
  let (authors, tags) = foldl tagParser ([], []) (T.splitOn "," rawTag)
  let manga =
        defaultManga
        { mangaName =
            head $
            fromDocument doc $// element "meta" >=>
            attributeIs "name" "subject" >=> attribute "content"
        , mangaAuthors = authors
        , mangaTags = tags
        }
  let links =
        catMaybes $
        fromDocument doc $// attributeIs "id" "vContent" &// element "a" >=> \x -> do
          let name = T.concat $ x $// content
          let link = head $ x $| attribute "href"
          return $
            if (not . T.null $ name) && ("archives" `T.isInfixOf` link)
              then Just $ Key $ lastToInt link
              else Nothing
  return (manga, links)
  where
    tagParser (as, ts) t =
      ( as ++ maybeToList (T.stripPrefix "A:" t)
      , ts ++ maybeToList (T.stripPrefix "G:" t))

chapterDetail :: CookieJar -> ChapterId -> IO (Maybe Chapter)
chapterDetail jar cid =
  retrying limitedBackoff (\_ b -> return $ isNothing b) $ \re ->
    chapterDetail' (show (rsIterNumber re)) jar cid

chapterDetail' :: String -> CookieJar -> ChapterId -> IO (Maybe Chapter)
chapterDetail' nocache jar cid = do
  req' <-
    parseRequest
      ("http://www.yuncomics.com/archives/" ++ show cid ++ "?549234" ++ nocache)
  let req =
        setRequestHeader "User-Agent" ["Android 5.0"] $
        setRequestHeader "Cache-Control" ["max-age=0"] $
        setRequestHeader "Upgrade-Insecure-Requests" ["1"] $
        req' {cookieJar = Just jar}
  body <- getResponseBody <$> requestWithRetry req
  let title = getTitle body
  let srcs = getDataSrcs body
  -- if null srcs
  --   then BL.putStrLn body
  --   else pure ()
  return $
    if (not . T.null) title && (not . null) srcs
      then Just Chapter {chapterName = title, chapterSrcs = srcs}
      else Nothing
  where
    getTitle :: BL.ByteString -> T.Text
    getTitle str =
      T.strip . T.decodeUtf8 . BL.toStrict . fst $
      BL.breakOn " | YUNCOMICS</title>" (snd (BL.breakAfter "<title>" str))
    getDataSrcs :: BL.ByteString -> [T.Text]
    getDataSrcs str = do
      let (src, left) = BL.breakOn "\"" . snd $ BL.breakAfter "data-src=\"" str
      if BL.null left
        then []
        else T.decodeUtf8 (BL.toStrict src) : getDataSrcs left

getCookieJar :: IO CookieJar
getCookieJar
  -- cloudflare cookie
 = do
  manager <- newManager tlsManagerSettings
  req' <- parseRequest "http://www.yuncomics.com/archives/213688?123124"
  let req1 =
        setRequestHeader "User-Agent" ["Android 5.0"] $
        setRequestHeader "Cache-Control" ["no-cache"] req'
  res1 <- httpLbs req1 manager
  let cookie = decryptCookie $ getResponseBody res1
  let jar1 =
        createCookieJar $ cookie : destroyCookieJar (responseCookieJar res1)
  -- login
  req' <-
    parseRequest "POST http://www.yuncomics.com/wp-login.php?action=postpass"
  let req2 =
        setRequestBodyURLEncoded
          [("post_password", "qndxkr"), ("Submit", "Submit")] $
        setRequestHeader "User-Agent" ["Android 5.0"]
           -- $ setRequestHeader "Cache-Control" ["no-cache"]
         $
        req' {cookieJar = Just jar1}
  res2 <- httpLbs req2 manager
  let jar2 = responseCookieJar res2
  return jar2
