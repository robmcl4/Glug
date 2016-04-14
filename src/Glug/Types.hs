{-# LANGUAGE DeriveGeneric #-}

module Glug.Types (
  MovieDetails (..)
, MovieSubtitles (..)
, IMDbId
, ApiKey
, WordCount (..)
, WordRank (..)
) where

import qualified Text.Subtitles.SRT as SRT
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Time.Clock as C

import Data.Aeson (ToJSON)
import Data.Int (Int32)
import GHC.Generics

-- | Information gained after scraping a movie's subtitles.
data MovieSubtitles = MovieSubtitles {
        imdbid :: TL.Text -- ^ The IMDb ID of the movie
      , subtitles :: SRT.Subtitles -- ^ The subtitles found
      } deriving (Eq, Show)


-- | Details about a movie from The Movie Database
data MovieDetails = MovieDetails {
        runtime :: Integer -- ^ The runtime of the movie, in seconds
      , poster :: T.Text -- ^ The path to the movie poster in TMDb's server
      , overview :: T.Text -- ^ An english description of the movie
      } deriving (Eq, Show, Generic)
instance ToJSON MovieDetails


-- | An ID from IMDb
type IMDbId = String


-- | The Api key for The Movie Database
type ApiKey = String


-- | A record of a given word's appearances in the movie
data WordCount = WordCount {
        text :: T.Text -- ^ The word
      , freq :: Int32 -- ^ Number of occurances of this word
      , occurances :: [C.DiffTime] -- ^ A List of times this word occurs
      } deriving (Show, Eq)


-- | A combination of a specific word and its heuristic ranking
data WordRank = WordRank {
        wordcount :: WordCount -- ^ The word
      , heuristic :: Int32 -- ^ The heuristic statistic, bigger is better
      } deriving (Show, Eq)
instance Ord WordRank where
  compare w1 w2 = compare (heuristic w1) (heuristic w2)
