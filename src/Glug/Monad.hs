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
, logM
, realTlsGetM
, throwError
) where


import qualified Data.ByteString.Lazy as BSL
import qualified Network.HTTP.Conduit as C
import qualified Data.Cache.LRU as LRU

import Control.Concurrent.MVar
import Control.Monad.Except
import Control.Monad.Reader as R
import Control.Monad.Trans.Reader as RT
import Control.Monad.Writer.Lazy
import Data.Time.Clock
import Data.Time.Format (formatTime, defaultTimeLocale)

import Glug.Constants (useragent)

-- -------------------------------- MonadGlugIO --------------------------------

newtype MonadGlugIO e a = MonadGlugIO {
    runMonadGlugIO :: ReaderT (MVar (LRU.LRU String BSL.ByteString)) (ExceptT e (WriterT [String] IO)) a
  } deriving (Functor, Applicative, Monad, MonadIO)


instance MonadWriter [String] (MonadGlugIO e) where
    writer = MonadGlugIO . writer
    tell =  MonadGlugIO . tell
    listen =  MonadGlugIO . listen . runMonadGlugIO
    pass = MonadGlugIO . pass . runMonadGlugIO


instance MonadError e (MonadGlugIO e) where
    throwError = MonadGlugIO . throwError
    m `catchError` f = MonadGlugIO $ runMonadGlugIO m `catchError` (runMonadGlugIO . f)

instance MonadReader (MVar (LRU.LRU String BSL.ByteString)) (MonadGlugIO e) where
    ask = MonadGlugIO RT.ask
    local f = MonadGlugIO . RT.local f . runMonadGlugIO


-- | Run MonadGlugIO and reduce to Either for a result and a message log
execMonadGlugIO :: MonadGlugIO e a -> IO (Either e a, [String])
execMonadGlugIO mgio = do
    mvr <- newMVar . LRU.newLRU $ Just 64
    execMonadGlugIOWithCache mvr mgio


-- | Run MonadGlugIO with a shared cache of external requests
execMonadGlugIOWithCache :: MVar (LRU.LRU String BSL.ByteString)
                            -- ^ the shared cache
                            -> MonadGlugIO e a
                            -- ^ the monad instance
                            -> IO (Either e a, [String])
                            -- ^ either the answer or an error, plus a message log
execMonadGlugIOWithCache mvr mgio = runWriterT . runExceptT $ runReaderT (runMonadGlugIO mgio) mvr


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

realTlsGetM :: String -> MonadGlugIO String BSL.ByteString
realTlsGetM url = do
    logM "realTlsGetM" $ "getting URL " ++ url
    mvr <- R.ask
    cache <- liftIO $ takeMVar mvr
    let (cache', bsl) = LRU.lookup url cache
    case bsl of
      Just x  -> do
        liftIO $ putMVar mvr cache'
        return x
      Nothing -> do
        liftIO $ putMVar mvr cache
        val <- _realTlsGetM url
        liftIO . modifyMVar_ mvr $ return . LRU.insert url val
        return val

_realTlsGetM :: String -> MonadGlugIO String BSL.ByteString
_realTlsGetM url = do
    initReq <- hoistMaybe "could not parse url" . C.parseRequest $ url
    mgr <- liftIO $ C.newManager C.tlsManagerSettings
    let req = initReq { C.requestHeaders = [("User-Agent", useragent)] }
    C.responseBody <$> C.httpLbs req mgr
