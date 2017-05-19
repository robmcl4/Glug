{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Glug.Types (
  MovieDetails (..)
, MovieSubtitles (..)
, IMDbId
, ApiKey
, WordCount (..)
, WordRank (..)
, Subtitle (..)
, Cache
, newCache
) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Time.Clock as C
import qualified Data.Cache.LRU as LRU
import qualified Data.ByteString.Lazy as BSL

import Control.Concurrent.MVar
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Aeson
import Data.Int (Int32)
import GHC.Generics

-- | Information gained after scraping a movie's subtitles.
data MovieSubtitles = MovieSubtitles {
        imdbid :: TL.Text -- ^ The IMDb ID of the movie
      , subtitles :: [Subtitle] -- ^ The subtitles found
      } deriving (Eq, Show)


-- | A single subtitle line.
data Subtitle = Subtitle {
        dialogue :: T.Text
      , timestamp :: C.DiffTime
      } deriving (Eq, Show)
instance ToJSON Subtitle where
  toJSON (Subtitle d t) = object ["dialogue" .= d, "timestamp" .= ts]
      where
            ts :: Integer
            ts = round t


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
      , freq :: Int32 -- ^ Number of occurrences of this word
      , occurrences :: [C.DiffTime] -- ^ A List of times this word occurs
      } deriving (Show, Eq)


-- | A combination of a specific word and its heuristic ranking
data WordRank = WordRank {
        wordcount :: WordCount -- ^ The word
      , heuristic :: Int32 -- ^ The heuristic statistic, bigger is better
      } deriving (Show, Eq)
instance Ord WordRank where
  compare w1 w2 = compare (heuristic w1) (heuristic w2)


-- | A cache of external network requests
type Cache = MVar (LRU.LRU String BSL.ByteString)

-- | Constructs a new Cache from the supplied max capacity of items
newCache :: MonadIO m => Integer -> m Cache
newCache = liftIO . newMVar . LRU.newLRU . Just
