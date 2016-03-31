{-# LANGUAGE DeriveGeneric #-}

module API.Helpers (
  TitleLink
, getTitles
) where

import qualified Data.Text.Lazy as T

import Data.Aeson
import GHC.Generics

import Downloader (candidateTitles)


data TitleLink = TitleLink { href :: T.Text
                           , title  :: T.Text
                           , subs  :: Integer }
                           deriving (Eq, Show, Generic)

instance ToJSON TitleLink


getTitles :: String -> IO (Either String [TitleLink])
getTitles s = do
    ettls <- candidateTitles s
    case ettls of
      Right ttls -> return . Right . map (\(a, b, c) -> TitleLink a b c) $ ttls
      Left x     -> return . Left $ x
