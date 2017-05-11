{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}


module Glug.Monad (
  MonadGlugIO (..)
, HttpGetM
, execMonadGlugIO
, hoistEither
, hoistMaybe
, logM
, realTlsGetM
) where


import qualified Data.ByteString.Lazy as BSL
import qualified Network.HTTP.Conduit as C

import Control.Monad.Except
import Control.Monad.Writer.Lazy
import Data.Bifunctor (first)
import Data.Time.Clock
import Data.Time.Format (formatTime, defaultTimeLocale)

import Glug.Constants (useragent)

-- -------------------------------- MonadGlugIO --------------------------------

newtype MonadGlugIO e a = MonadGlugIO {
    runMonadGlugIO :: ExceptT e (WriterT [String] IO) a
  } deriving (Functor, Applicative, Monad, MonadIO)


instance MonadWriter [String] (MonadGlugIO e) where
    writer = MonadGlugIO . writer
    tell =  MonadGlugIO . tell
    listen =  MonadGlugIO . listen . runMonadGlugIO
    pass = MonadGlugIO . pass . runMonadGlugIO


instance MonadError e (MonadGlugIO e) where
    throwError = MonadGlugIO . throwError
    m `catchError` f = MonadGlugIO $ runMonadGlugIO m `catchError` (runMonadGlugIO . f)


-- | Run MonadGlugIO and reduce to Either for a result and a message log
execMonadGlugIO :: MonadGlugIO e a -> IO (Either e a, [String])
execMonadGlugIO = runWriterT . runExceptT . runMonadGlugIO


-- | Convert an Either into a MonadError
hoistEither :: MonadError e m => Either e a -> m a
hoistEither (Right x) = return x
hoistEither (Left x) = throwError x


-- | Convert a Maybe into a MonadError, using the given error when Nothing is found.
hoistMaybe :: MonadError e m => e -> Maybe a -> m a
hoistMaybe _ (Just x) = return x
hoistMaybe e Nothing  = throwError e


-- | Log a message with a module tag, also embeds timestamp
logM :: (MonadIO m, MonadWriter [String] m) => String -> String -> m ()
logM tag msg = do
    t <- liftIO $ formatTime defaultTimeLocale "%FT%X" <$> getCurrentTime
    tell [t ++ " [" ++ tag ++ "] " ++ msg]


-- ------------------------------ Network Requests -----------------------------

type HttpGetM =  forall m. (MonadIO m, MonadError String m, MonadWriter [String] m) => String -> m BSL.ByteString

realTlsGetM :: HttpGetM
realTlsGetM url = do
    logM "realTlsGetM" $ "getting URL " ++ url
    initReq <- hoistEither . first show . C.parseUrl $ url
    mgr <- liftIO $ C.newManager C.tlsManagerSettings
    let req = initReq { C.requestHeaders = [("User-Agent", useragent)] }
    C.responseBody <$> C.httpLbs req mgr
