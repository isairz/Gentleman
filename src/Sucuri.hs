{-# LANGUAGE OverloadedStrings #-}

module Sucuri
    ( decode
    -- , findSucuri
    ) where

import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.ByteString       as AP
import           Data.Attoparsec.ByteString.Char8 as AP8
import qualified Data.ByteString.Base64           as Base64
import qualified Data.ByteString.Char8            as B8
import           Data.Char                        (chr)
-- import           Text.Regex.Posix                 ((=~))
-- import           Text.Regex.Posix.ByteString

-- findSucuri :: B8.ByteString -> B8.ByteString
-- findSucuri =
--   where pat = "sucuri_cloudproxy_js='',S='([^']+)" :: B8.ByteString
--         (_, _, _, a:_) = body =~ pat

decode :: B8.ByteString -> B8.ByteString
decode base64 =
  case Base64.decode base64 of
    Left err -> B8.pack err
    Right js -> case parseOnly (sucuri <* endOfInput) (B8.filter (not . isSpace) js) of
                  Left err     -> B8.pack err
                  Right result -> result

sucuri :: Parser B8.ByteString
sucuri = do
  AP8.take 2 -- 'b='
  b <- expr
  string ";document.cookie="
  a <- expr
  AP8.take 3 -- '+b+'
  c <- expr
  string ";location.reload();"
  return $ a `B8.append` b `B8.append` c

expr :: Parser B8.ByteString
expr = B8.concat <$> sepBy term (char '+')

term :: Parser B8.ByteString
term = base >>= func

base :: Parser B8.ByteString
base = between '"'
   <|> between '\''
   <|> (B8.singleton . chr) <$> (string "String.fromCharCode(" *> num <* char ')')
   where between c = char c *> AP8.takeWhile (/= c) <* char c

func :: B8.ByteString -> Parser B8.ByteString
func s = charAt s
     <|> slice s
     <|> substr s
     <|> pure s

-- charAt = (<$> (string ".charAt(" *> num <* char ')')) . (B8.singleton .) . B8.index
charAt :: B8.ByteString -> Parser B8.ByteString
charAt s = do
  at <- string ".charAt(" *> num <* char ')'
  return $ B8.singleton $ B8.index s at

slice :: B8.ByteString -> Parser B8.ByteString
slice s = do
  string ".slice("
  from <- num
  char ','
  to <- num
  char ')'
  return $ B8.take (to - from) $ B8.drop from s

substr :: B8.ByteString -> Parser B8.ByteString
substr s = do
  string ".substr("
  start <- num
  char ','
  len <- num
  char ')'
  return $ B8.take len $ B8.drop start s

num :: Parser Int
num = string "0x" *> hexadecimal
  <|> decimal
