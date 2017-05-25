{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}


module Glug.Monad (
  MonadGlugIO (..)
, execMonadGlugIO
, execMonadGlugIOWithCache
, hoistEither
, hoistMaybe
, liftIO
, logM
, realTlsGetUrl
, throwError
) where


import qualified Data.ByteString.Lazy as BSL
import qualified Network.HTTP.Conduit as C

import Control.Monad.Except
import Control.Monad.Reader as R
import Control.Monad.Trans.Reader as RT
import Control.Monad.Writer.Lazy
import Data.Time.Clock
import Data.Time.Format (formatTime, defaultTimeLocale)

import Glug.Constants (useragent, defaultLRUSize)
import Glug.Types (Cache, newCache)

-- -------------------------------- MonadGlugIO --------------------------------

newtype MonadGlugIO e a = MonadGlugIO {
    runMonadGlugIO :: ReaderT Cache (ExceptT e (WriterT [String] IO)) a
  } deriving (Functor, Applicative, Monad, MonadIO)


instance MonadWriter [String] (MonadGlugIO e) where
    writer = MonadGlugIO . writer
    tell =  MonadGlugIO . tell
    listen =  MonadGlugIO . listen . runMonadGlugIO
    pass = MonadGlugIO . pass . runMonadGlugIO


instance MonadError e (MonadGlugIO e) where
    throwError = MonadGlugIO . throwError
    m `catchError` f = MonadGlugIO $ runMonadGlugIO m `catchError` (runMonadGlugIO . f)


instance MonadReader Cache (MonadGlugIO e) where
    ask = MonadGlugIO RT.ask
    local f = MonadGlugIO . RT.local f . runMonadGlugIO


-- | Run MonadGlugIO and reduce to Either for a result and a message log
execMonadGlugIO :: MonadGlugIO e a -> IO (Either e a, [String])
execMonadGlugIO mgio = do
    cache <- newCache defaultLRUSize
    execMonadGlugIOWithCache cache mgio


-- | Run MonadGlugIO with a shared cache of external requests
execMonadGlugIOWithCache :: Cache
                            -- ^ the shared cache
                            -> MonadGlugIO e a
                            -- ^ the monad instance
                            -> IO (Either e a, [String])
                            -- ^ either the answer or an error, plus a message log
execMonadGlugIOWithCache cache mgio = runWriterT . runExceptT $ runReaderT (runMonadGlugIO mgio) cache


-- | Convert an Either into a MonadError
hoistEither :: MonadError e m => Either e a -> m a
hoistEither (Right x) = return x
hoistEither (Left x) = throwError x


-- | Convert a Maybe into a MonadError, using the given error when Nothing is found.
hoistMaybe :: MonadError e m => e -> Maybe a -> m a
hoistMaybe _ (Just x) = return x
hoistMaybe e Nothing  = throwError e


-- | Log a message with a module tag, also embeds timestamp
logM :: String -> String -> MonadGlugIO a ()
logM tag msg = do
    t <- liftIO $ formatTime defaultTimeLocale "%FT%X" <$> getCurrentTime
    tell [t ++ " [" ++ tag ++ "] " ++ msg]


-- ------------------------------ Network Requests -----------------------------

realTlsGetUrl :: String -> MonadGlugIO String BSL.ByteString
realTlsGetUrl url = do
    logM "realTlsGetUrl" $ "getting URL " ++ url
    initReq <- hoistMaybe "could not parse url" . C.parseRequest $ url
    mgr <- liftIO $ C.newManager C.tlsManagerSettings
    let req = initReq { C.requestHeaders = [("User-Agent", useragent)] }
    C.responseBody <$> C.httpLbs req mgr
