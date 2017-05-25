{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module API.Helpers (
  getTitles
, getBestWords
, getTitleDetails
, isImdbId
) where

import qualified Data.ByteString.Base64.URL.Lazy as B64
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as ENC
import qualified Data.Text as TS
import qualified System.Environment as ENV


import Control.Monad.IO.Class (liftIO)
import Control.Monad.Error.Class (throwError)
import Data.Aeson
import Data.Char (isDigit)
import Data.List (isPrefixOf)
import GHC.Generics


import qualified Glug as G


data TitleLink = TitleLink { ref   :: T.Text
                           , title :: T.Text
                           , subs  :: Integer }
                           deriving (Eq, Show, Generic)
instance ToJSON TitleLink


data MovieSummary = MovieSummary { imdbid :: T.Text
                                 , ranked_words :: [RankedWord]
                                 , runtime :: Integer
                                 , first_subtitles :: [G.Subtitle] }
                                 deriving (Eq, Show, Generic)
instance ToJSON MovieSummary


data RankedWord = RankedWord { word :: T.Text
                             , occurrences :: [Integer] }
                             deriving (Eq, Show, Generic)
instance ToJSON RankedWord


getTitles :: String -> G.MonadGlugIO String [TitleLink]
getTitles s = do
    result <- map mkTitleLink <$> G.candidateTitles s
    return result
  where mkTitleLink (a, b, c) = TitleLink { ref = toBase64 a, title = b, subs = c }


getBestWords :: String -> (Integer, Integer) -> G.MonadGlugIO String MovieSummary
getBestWords refsz rng = case fromBase64 refsz of
        Left  s   -> throwError s
        Right url -> if not $ isSubLink url
            then throwError "not subscene url"
            else do
              mov <- G.getSubtitles url
              let wcs = G.countWords . G.subtitles $ mov
              let best = map toRW . take 25 $ G.bestCandidates wcs rng
              let rt = round . toRational . maximum . concatMap G.occurrences $ wcs
              return MovieSummary { imdbid = G.imdbid mov
                                  , ranked_words = best
                                  , runtime = rt
                                  , first_subtitles = take 10 . G.subtitles $ mov }
  where toRW wr = RankedWord { word = T.fromStrict . G.text . G.wordcount $ wr
                             , occurrences = map (round . toRational) . G.occurrences . G.wordcount $ wr
                             }


getTitleDetails :: String -> G.MonadGlugIO String G.MovieDetails
getTitleDetails i = do
    maybeKey <- liftIO $ getTMDbKey
    key <- G.hoistMaybe "The Movie Database env variable not set" maybeKey
    G.getDetailsOfMovie i key


isSubLink :: String -> Bool
isSubLink t = "/subtitles/" `isPrefixOf` t


isImdbId :: TS.Text -> Bool
isImdbId t = "tt" `TS.isPrefixOf` t && TS.all isDigit (TS.drop 2 t)


getTMDbKey :: IO (Maybe G.ApiKey)
getTMDbKey = ENV.lookupEnv "TMDB_KEY"

toBase64 :: T.Text -> T.Text
toBase64 = ENC.decodeUtf8 . B64.encode . ENC.encodeUtf8

fromBase64 :: String -> Either String String
fromBase64 s = do
    bs <- B64.decode . ENC.encodeUtf8 . T.pack $ s
    case ENC.decodeUtf8' bs of
      Left _ -> Left "Could not decode ref"
      Right t -> Right . T.unpack $ t
