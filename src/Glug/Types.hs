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


data MovieSubtitles = MovieSubtitles { imdbid :: TL.Text
                                     , subtitles :: SRT.Subtitles }
                                     deriving (Eq, Show)


data MovieDetails = MovieDetails { runtime :: Integer
                                 , poster :: T.Text
                                 , overview :: T.Text }
                                 deriving (Eq, Show, Generic)
instance ToJSON MovieDetails


type IMDbId = String


type ApiKey = String


data WordCount = WordCount { text :: T.Text
                           , freq :: Int32
                           , occurances :: [C.DiffTime]
                           } deriving (Show, Eq)


data WordRank = WordRank { wordcount :: WordCount
                        , heuristic :: Int32 }
                        deriving (Show, Eq)
instance Ord WordRank where
  compare w1 w2 = compare (heuristic w1) (heuristic w2)
