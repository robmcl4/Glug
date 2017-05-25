{-# LANGUAGE FlexibleContexts #-}

module Glug.Cache (
  tlsGetUrl
, serializeCache
, loadCache
) where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Cache.LRU as LRU

import Control.Concurrent.MVar
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ask)
import Control.Monad.Except (MonadError)
import Data.Serialize

import Glug.Constants (defaultLRUSize)
import Glug.Monad
import Glug.Types (Cache)


-- | Reads the cache for a URL's content. If not found, issues a real request
tlsGetUrl :: String
              -- ^ The URL to retrieve
              -> MonadGlugIO String BSL.ByteString
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


_readLru :: (MonadIO m, MonadReader Cache m) => m (LRU.LRU String BSL.ByteString)
_readLru = ask >>= liftIO . readMVar


_takeLru :: (MonadIO m, MonadReader Cache m) => m (LRU.LRU String BSL.ByteString)
_takeLru = ask >>= liftIO . takeMVar


_putLru :: (MonadIO m, MonadReader Cache m) => LRU.LRU String BSL.ByteString -> m ()
_putLru lru = ask >>= \mvar -> liftIO $ putMVar mvar lru


_modifyLru' :: (MonadIO m, MonadReader Cache m) =>
        (LRU.LRU String BSL.ByteString -> LRU.LRU String BSL.ByteString)
        -> m ()
_modifyLru' f = _takeLru >>= return . f >>= _putLru


-- Serialized Cache Methods ----------------------------------------------------

-- | Serialize the internal cache state
serializeCache :: (MonadIO m, MonadReader Cache m) => m BSL.ByteString
serializeCache = encodeLru <$> _readLru


-- | Load internal cache from serialized state.
loadCache :: (MonadError String m, MonadIO m, MonadReader Cache m) => BSL.ByteString -> m ()
loadCache bsl = do
    -- the cache should NOT be held before invoking!
    decoded <- hoistEither . decodeLru $ bsl
    mvr <- ask
    _ <- liftIO $ swapMVar mvr decoded
    return ()


-- | Serialize an LRU cache
encodeLru :: LRU.LRU String BSL.ByteString -> BSL.ByteString
encodeLru = encodeLazy . LRU.toList


-- | Deserialize an LRU cache
decodeLru :: BSL.ByteString -> Either String (LRU.LRU String BSL.ByteString)
decodeLru = fmap (LRU.fromList (Just defaultLRUSize)) . decodeLazy
