{-# LANGUAGE FlexibleContexts #-}

module Glug.Cache (
  tlsGetUrl
) where

import qualified Data.Cache.LRU as LRU
import qualified Data.ByteString.Lazy as BSL

import Control.Concurrent.MVar
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ask)

import Glug.Monad
import Glug.Types (Cache)


-- | Reads the cache for a URL's content. If not found, issues a real request
tlsGetUrl :: String ->
              -- ^ The URL to retrieve
              MonadGlugIO String BSL.ByteString
              -- ^ Either the result or an error
tlsGetUrl url = do
    lru <- _takeLru
    case LRU.lookup url lru of
      (lru', Just x)  -> do
        -- cache hit
        _putLru lru'
        return x
      (lru', Nothing) -> do
        -- cache miss
        _putLru lru'
        val <- realTlsGetUrl url
        _modifyLru' $ LRU.insert url val
        return val


_takeLru :: (MonadIO m, MonadReader Cache m) => m (LRU.LRU String BSL.ByteString)
_takeLru = ask >>= liftIO . takeMVar


_putLru :: (MonadIO m, MonadReader Cache m) => LRU.LRU String BSL.ByteString -> m ()
_putLru lru = ask >>= \mvar -> liftIO $ putMVar mvar lru


_modifyLru' :: (MonadIO m, MonadReader Cache m) =>
        (LRU.LRU String BSL.ByteString -> LRU.LRU String BSL.ByteString)
        -> m ()
_modifyLru' f = _takeLru >>= return . f >>= _putLru
