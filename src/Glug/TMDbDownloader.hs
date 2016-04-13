{-# LANGUAGE OverloadedStrings #-}


module Glug.TMDbDownloader (
  getDetailsOfMovie
) where


import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Network.HTTP.Conduit as C

import Control.Monad.Except
import Data.Aeson

import Glug.Constants (useragent)
import Glug.Types (MovieDetails (..), IMDbId, ApiKey)


tmdb_base :: String
tmdb_base = "https://api.themoviedb.org/3/"


getDetailsOfMovie :: IMDbId -> ApiKey -> IO (Either String MovieDetails)
getDetailsOfMovie i k = runExceptT $ do
    bsl <- makeGet $ tmdb_base ++ "find/" ++ i ++ "?external_source=imdb_id&api_key=" ++ k
    obj <- dec' bsl
    id_ <- getMovie obj >>= lookupMovieId
    bsl' <- makeGet $ tmdb_base ++ "movie/" ++ (show id_) ++ "?api_key=" ++ k
    obj' <- dec' bsl'
    post_path <- lookupPosterPath obj'
    mov_len <- lookupMovieLength obj'
    ovv <- lookupOverview obj'
    return MovieDetails { runtime = mov_len, poster = post_path, overview = ovv }
  where dec' bs = case (decode bs :: Maybe Value) of
                         Just (Object o) -> return o
                         _                -> throwError "No object found"
        getMovie hm = do
          v <- lookupArray "movie_results" hm
          if V.null v
            then throwError "No movie results"
            else case V.head v of
              (Object o) -> return o
              _          -> throwError "Did not find movie object"


makeGet :: String -> ExceptT String IO BSL.ByteString
makeGet url = do
    initReq <- C.parseUrl url
    mgr <- manager
    let req = initReq { C.requestHeaders = [("User-Agent", useragent)] }
    (liftM C.responseBody) $ C.httpLbs req mgr


manager :: MonadIO m => m C.Manager
manager = liftIO $ C.newManager C.tlsManagerSettings


lookupMovieId :: Object -> ExceptT String IO Integer
lookupMovieId o = case HM.lookup "id" o of
                        Just (Number s) -> return . round . toRational $ s
                        _               -> throwError "Did not find id"


lookupPosterPath :: Object -> ExceptT String IO T.Text
lookupPosterPath o = case HM.lookup "poster_path" o of
                         Just (String t) -> return t
                         _               -> throwError "Did not find poster path"


lookupMovieLength :: Object -> ExceptT String IO Integer
lookupMovieLength o = case HM.lookup "runtime" o of
                        Just (Number s) -> return . ((*) 60) . round . toRational $ s
                        _               -> throwError "Did not find id"


lookupOverview :: Object -> ExceptT String IO T.Text
lookupOverview o = case HM.lookup "overview" o of
                       Just (String t) -> return t
                       _               -> throwError "Did not find overview"


lookupArray :: T.Text -> Object -> ExceptT String IO Array
lookupArray k hm = case HM.lookup k hm of
                   Just (Array a) -> return $ a
                   _               -> throwError $ "Could not find array with key: " ++ T.unpack k
