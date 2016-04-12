{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module API.Helpers (
  TitleLink
, getTitles
, getBestWords
, getTitleDetails
, isSubLink
, isImdbId
) where

import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy as T
import qualified Data.Text as TS
import qualified Glug.WordCounter as WC
import qualified Glug.WordHeuristics as WH
import qualified System.Environment as ENV

import Control.Monad (liftM)
import Data.Aeson
import Data.Char (isDigit)
import GHC.Generics

import qualified Glug.SubsceneDownloader as SD
import qualified Glug.TMDbDownloader as TD


data TitleLink = TitleLink { href :: T.Text
                           , title  :: T.Text
                           , subs  :: Integer }
                           deriving (Eq, Show, Generic)
instance ToJSON TitleLink


data MovieSummary = MovieSummary { imdbid :: T.Text
                                 , ranked_words :: [RankedWord]
                                 , runtime :: Integer }
                                 deriving (Eq, Show, Generic)
instance ToJSON MovieSummary


data RankedWord = RankedWord { word :: T.Text
                             , occurances :: [Integer] }
                             deriving (Eq, Show, Generic)
instance ToJSON RankedWord


getTitles :: String -> IO (Either String [TitleLink])
getTitles s = do
    ettls <- SD.candidateTitles s
    case ettls of
      Right ttls -> return . Right . map (\(a, b, c) -> TitleLink a b c) $ ttls
      Left x     -> return . Left $ x


getBestWords :: String -> (Integer, Integer) -> IO (Either String MovieSummary)
getBestWords url rng = do
    mov <- SD.getSubtitles url
    return $ do
        mov' <- mov
        let wcs = WC.countWords . SD.subtitles $ mov'
        let best = (map toRW . take 25 . (flip WH.bestCandidates) rng) $ wcs
        let runtime = round . toRational . maximum . concat . map (WC.occurances) $ wcs
        return MovieSummary { imdbid = SD.imdbid mov'
                            , ranked_words = best
                            , runtime = runtime }
  where toRW wr = RankedWord (T.fromStrict . WC.text . WH.wordcount $ wr)
                             (map (round . toRational) . WC.occurances . WH.wordcount $ wr)


getTitleDetails :: String -> IO (Either String TD.MovieDetails)
getTitleDetails i = do
    key <- getTMDbKey
    case key of
      Nothing -> return . Left $ "The Movie Database env variable not set"
      Just k  -> TD.getDetailsOfMovie i k


isSubLink :: TS.Text -> Bool
isSubLink t = "/subtitles/" `TS.isPrefixOf` t


isImdbId :: TS.Text -> Bool
isImdbId t = "tt" `TS.isPrefixOf` t && TS.all isDigit (TS.drop 2 t)


getTMDbKey :: IO (Maybe TD.ApiKey)
getTMDbKey = ENV.lookupEnv "TMDB_KEY"
