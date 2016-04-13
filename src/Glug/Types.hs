{-# LANGUAGE DeriveGeneric #-}

module Glug.Types (
  MovieDetails (..)
, MovieSubtitles (..)
, IMDbId
, ApiKey
, WordCount (..)
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
