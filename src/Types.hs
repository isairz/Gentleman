module Types
    ( Manga(..)
    ) where

data Manga = Manga
  { link  :: String
  , title :: String
  -- , Author :: String
  } deriving (Show, Eq)
