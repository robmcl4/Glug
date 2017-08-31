{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Glug.Monad (
  MonadGlugIO (..)
, execMonadGlugIO
, hoistEither
, hoistMaybe
) where

import Control.Monad.Except
import Control.Monad.Trans.State.Lazy as ST
import Control.Monad.State.Class as S

import Glug.Cache (Cache)

-- -------------------------------- MonadGlugIO --------------------------------

newtype MonadGlugIO e a = MonadGlugIO {
    runMonadGlugIO :: ExceptT e (StateT Cache IO) a
  } deriving (Functor, Applicative, Monad, MonadIO)


instance MonadError e (MonadGlugIO e) where
    throwError = MonadGlugIO . throwError
    m `catchError` f = MonadGlugIO $ runMonadGlugIO m `catchError` (runMonadGlugIO . f)

instance MonadState Cache (MonadGlugIO e) where
    get = MonadGlugIO S.get
    put = MonadGlugIO . S.put


-- | Run MonadGlugIO and reduce to Either for a result, plus modified cache
execMonadGlugIO :: Cache -> MonadGlugIO e a -> IO (Either e a, Cache)
execMonadGlugIO cache mgio = runStateT (runExceptT . runMonadGlugIO $ mgio) cache


-- | Convert an Either into a MonadError
hoistEither :: MonadError e m => Either e a -> m a
hoistEither (Right x) = return x
hoistEither (Left x) = throwError x


-- | Convert a Maybe into a MonadError, using the given error when Nothing is found.
hoistMaybe :: MonadError e m => e -> Maybe a -> m a
hoistMaybe _ (Just x) = return x
hoistMaybe e Nothing  = throwError e
