{-# LANGUAGE OverloadedStrings #-}

module ROD.Gentleman.Site.CloudFlare
  ( decode
  , decryptCookie
  ) where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString as AP
import Data.Attoparsec.ByteString.Char8 as AP8
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Lazy.Search as BL
import Data.Char (chr)

import Data.Time.Calendar
import Data.Time.Clock
import Network.HTTP.Conduit as Http

decryptCookie :: BL.ByteString -> Cookie
decryptCookie html =
  Cookie
  { cookie_name = cookieName
  , cookie_value = cookieValue
  , cookie_expiry_time = future
  , cookie_domain = "www.yuncomics.com"
  , cookie_path = "/"
  , cookie_creation_time = past
  , cookie_last_access_time = past
  , cookie_persistent = False
  , cookie_host_only = False
  , cookie_secure_only = False
  , cookie_http_only = False
  }
  where
    past = UTCTime (ModifiedJulianDay 56200) (secondsToDiffTime 0)
    future = UTCTime (ModifiedJulianDay 562000) (secondsToDiffTime 0)
    cookieName:cookieValue:_ =
      B.split '=' . decode . BL.toStrict . BL.takeWhile (/= '\'') . snd $
      BL.breakAfter "sucuri_cloudproxy_js='',S='" html

decode :: B.ByteString -> B.ByteString
decode base64 =
  case Base64.decode base64 of
    Left err -> B.pack err
    Right js ->
      case parseOnly (sucuri <* endOfInput) (B.filter (not . isSpace) js) of
        Left err -> B.pack err
        Right result -> result

sucuri :: Parser B.ByteString
sucuri = do
  AP8.take 2 -- 'b='
  b <- expr
  string ";document.cookie="
  a <- expr
  AP8.take 3 -- '+b+'
  c <- expr
  string ";location.reload();"
  return $ a `B.append` b -- `B.append` c

expr :: Parser B.ByteString
expr = B.concat <$> sepBy term (char '+')

term :: Parser B.ByteString
term = base >>= func

base :: Parser B.ByteString
base =
  between '"' <|> between '\'' <|>
  (B.singleton . chr) <$> (string "String.fromCharCode(" *> num <* char ')')
  where
    between c = char c *> AP8.takeWhile (/= c) <* char c

func :: B.ByteString -> Parser B.ByteString
func s = charAt s <|> slice s <|> substr s <|> pure s

-- charAt = (<$> (string ".charAt(" *> num <* char ')')) . (B.singleton .) . B.index
charAt :: B.ByteString -> Parser B.ByteString
charAt s = do
  at <- string ".charAt(" *> num <* char ')'
  return $ B.singleton $ B.index s at

slice :: B.ByteString -> Parser B.ByteString
slice s = do
  string ".slice("
  from <- num
  char ','
  to <- num
  char ')'
  return $ B.take (to - from) $ B.drop from s

substr :: B.ByteString -> Parser B.ByteString
substr s = do
  string ".substr("
  start <- num
  char ','
  len <- num
  char ')'
  return $ B.take len $ B.drop start s

num :: Parser Int
num = string "0x" *> hexadecimal <|> decimal
