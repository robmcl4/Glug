{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module API.Helpers (
  TitleLink
, getTitles
, getBestWords
, isSubLink
) where

import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy as T
import qualified Data.Text as TS
import qualified WordCounter as WC
import qualified WordHeuristics as WH

import Data.Aeson
import GHC.Generics

import Downloader (candidateTitles, getSubtitles)


data TitleLink = TitleLink { href :: T.Text
                           , title  :: T.Text
                           , subs  :: Integer }
                           deriving (Eq, Show, Generic)
instance ToJSON TitleLink


data RankedWord = RankedWord { word :: T.Text
                             , occurances :: [Integer] }
                             deriving (Eq, Show, Generic)
instance ToJSON RankedWord


getTitles :: String -> IO (Either String [TitleLink])
getTitles s = do
    ettls <- candidateTitles s
    case ettls of
      Right ttls -> return . Right . map (\(a, b, c) -> TitleLink a b c) $ ttls
      Left x     -> return . Left $ x


getBestWords :: String -> (Integer, Integer) -> IO (Either String [RankedWord])
getBestWords url rng = do
    esubs <- getSubtitles url
    let wrs = esubs >>= Right . WC.countWords >>= Right . (flip WH.bestCandidates) rng
    return $ wrs >>= Right . map toRW . take 25
  where toRW wr = RankedWord (T.fromStrict . WC.text . WH.wordcount $ wr)
                             (map (round . toRational) . WC.occurances . WH.wordcount $ wr)


isSubLink :: TS.Text -> Bool
isSubLink t =    "/subtitles/" `TS.isPrefixOf` t
