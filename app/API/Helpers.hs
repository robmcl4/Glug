{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module API.Helpers (
  getTitles
, getBestWords
, getTitleDetails
, isImdbId
) where

import Data.ByteString.Base64.URL.Lazy as B64
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as ENC
import qualified Data.Text as TS
import qualified System.Environment as ENV

import Data.Aeson
import Data.Char (isDigit)
import Data.List (isPrefixOf)
import GHC.Generics


import qualified Glug as G
import qualified Glug.Monad as GM


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


getTitles :: String -> IO (Either String [TitleLink])
getTitles s = do
    fmap (fmap (fmap mkTitleLink)) $ G.candidateTitles s
  where mkTitleLink (a, b, c) = TitleLink { ref = toBase64 a, title = b, subs = c }


getBestWords :: String -> (Integer, Integer) -> IO (Either String MovieSummary)
getBestWords refsz rng = do
    case fromBase64 refsz of
        Left  s   -> return . Left $ s
        Right url -> do
          if not $ isSubLink url
            then return . Left $ "not subscene url"
            else do
              mov <- G.getSubtitles url
              return $ do
                  mov' <- mov
                  let wcs = G.countWords . G.subtitles $ mov'
                  let best = (map toRW . take 25 . (flip G.bestCandidates) rng) $ wcs
                  let rt = round . toRational . maximum . concat . map (G.occurrences) $ wcs
                  return MovieSummary { imdbid = G.imdbid mov'
                                      , ranked_words = best
                                      , runtime = rt
                                      , first_subtitles = take 10 . G.subtitles $ mov' }
  where toRW wr = RankedWord { word = T.fromStrict . G.text . G.wordcount $ wr
                             , occurrences = map (round . toRational) . G.occurrences . G.wordcount $ wr
                             }


getTitleDetails :: String -> IO (Either String G.MovieDetails)
getTitleDetails i = do
    key <- getTMDbKey
    case key of
      Nothing -> return . Left $ "The Movie Database env variable not set"
      Just k  -> G.getDetailsOfMovie i k GM.realTlsGetM


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
