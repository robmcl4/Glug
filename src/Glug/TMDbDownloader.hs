{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}


module Glug.TMDbDownloader (
  getDetailsOfMovie
) where


import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V

import Control.Monad.Except
import Data.Aeson

import Glug.Types (MovieDetails (..), IMDbId, ApiKey)
import Glug.Monad (execMonadGlugIO, realTlsGetM)


tmdbBase :: String
tmdbBase = "https://api.themoviedb.org/3/"


-- | Gets details about a movie by IMDb Id
getDetailsOfMovie :: IMDbId
                     -- ^ The IMDb ID
                     -> ApiKey
                     -- ^ A (stubbable) function for getting HTTP requests
                     -> IO (Either String MovieDetails)
                     -- ^ Either an error message or movie details
getDetailsOfMovie i k = fmap fst . execMonadGlugIO $ do
    bsl <- realTlsGetM $ tmdbBase ++ "find/" ++ i ++ "?external_source=imdb_id&api_key=" ++ k
    id_ <- dec' bsl >>= getMovie >>= lookupInt "id"
    bsl' <- realTlsGetM $ tmdbBase ++ "movie/" ++ show id_ ++ "?api_key=" ++ k
    obj <- dec' bsl'
    post_path <- lookupText "poster_path" obj
    mov_len <- (60 *) <$> lookupInt "runtime" obj
    ovv <- lookupText "overview" obj
    return MovieDetails { runtime = mov_len, poster = post_path, overview = ovv }
  where dec' bs = case (decode bs :: Maybe Value) of
                         Just (Object o) -> return o
                         _               -> throwError "No object found"
        getMovie hm = do
          v <- lookupArray "movie_results" hm
          if V.null v
            then throwError "No movie results"
            else case V.head v of
              (Object o) -> return o
              _          -> throwError "Did not find movie object"


lookupInt :: MonadError String m => T.Text -> Object -> m Integer
lookupInt k o = case HM.lookup k o of
                    Just (Number s) -> return . round . toRational $ s
                    _               -> throwError $ "Did not find number " ++ T.unpack k


lookupText :: MonadError String m => T.Text -> Object -> m T.Text
lookupText k o = case HM.lookup k o of
                    Just (String s) -> return s
                    _               -> throwError $ "Did not find text " ++ T.unpack k


lookupArray :: MonadError String m => T.Text -> Object -> m Array
lookupArray k o = case HM.lookup k o of
                    Just (Array a) -> return a
                    _              -> throwError $ "Did not find array " ++ T.unpack k
