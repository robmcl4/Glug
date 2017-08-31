{-# LANGUAGE FlexibleContexts #-}

module Glug.Cache (
  Cache ()
, deserializeCache
, insertHardCache
, newCache
, mergeCache
, serializeCache
, tlsGetUrl
) where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map
import qualified Data.Serialize as Ser

import Control.Monad (mplus)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Except (MonadError)
import Control.Monad.State.Class (MonadState, get, put)
import System.Random (StdGen, mkStdGen, next)

import Glug.Net (realTlsGetUrl)


-- | A random-eviction cache
data Cache = Cache { kvStore :: Map.Map String BSL.ByteString
                   , randomGen :: StdGen
                   , maxCapacity :: Int
                   , auxKvStore :: Map.Map String BSL.ByteString
                   } deriving (Show)


-- | Makes a new cache from the given max capacity
newCache :: Int
            -- ^ Max capacity, nonpositive values are rounded up to 1
            -> Cache
            -- ^ The cache
newCache cap = Cache { kvStore = Map.empty
                     , randomGen = mkStdGen 179089975 -- TODO make this a param or system IO input?
                     , maxCapacity = max cap 1
                     , auxKvStore = Map.empty }


-- | Merges two maps together. Randomly evicts from source as dest is filled.
mergeCache :: Cache
              -- ^ the destination cache
              -> Cache
              -- ^ the source cache
              -> Cache
              -- ^ result
mergeCache c1 c2 = Map.foldrWithKey insertHardCache c1 $ auxKvStore c2


-- | Inserts a key-value pair into the short-term cache
insertSoftCache :: String -> BSL.ByteString -> Cache -> Cache
insertSoftCache k v c = c { auxKvStore = Map.insert k v (auxKvStore c) }


-- | Inserts a key-value pair into the long-term cache, evicting if necessary
insertHardCache :: String -> BSL.ByteString -> Cache -> Cache
insertHardCache k v c = if (Map.size . kvStore $ c) >= maxCapacity c
                          then insertHardCache k v . evictFromCache $ c
                          else c { kvStore = Map.insert k v (kvStore c) }


-- | Evicts one element from the given cache
evictFromCache :: Cache -> Cache
evictFromCache c = if Map.null kv
                     then c
                     else c { kvStore = kv', randomGen = gen' }
    where kv = kvStore c
          (i, gen') = next . randomGen $ c
          idx = i `mod` Map.size kv
          kv' = Map.deleteAt idx kv


-- | Look up a value in the cache
lookupCache :: String -> Cache -> Maybe BSL.ByteString
lookupCache s c = (Map.lookup s . auxKvStore $ c) `mplus` (Map.lookup s . kvStore $ c)


-- | Reads the cache for a URL's content. If not found, issues a real request
tlsGetUrl :: (MonadError String m, MonadIO m, MonadState Cache m) =>
                  String
                  -- ^ the URL to request
                  -> m BSL.ByteString
                  -- ^ the response (cache is modified)
tlsGetUrl url = get >>= \cache -> case lookupCache url cache of
                                    Just x  -> -- cache hit
                                      return x
                                    Nothing -> do
                                      -- cache miss
                                      val <- realTlsGetUrl url
                                      put $ insertSoftCache url val cache
                                      return val


-- | Serialize a cache
serializeCache :: Cache -> BSL.ByteString
serializeCache c = Ser.encodeLazy (fst . next . randomGen $ c, kvs)
    where c' = mergeCache c c -- puts the aux store into the hard store
          kvs = Map.toList . kvStore $ c'


-- | Deserialize a cache
deserializeCache :: Int -> BSL.ByteString -> Either String Cache
deserializeCache size bs = case Ser.decodeLazy bs of
                             Right (seed, kvs) -> Right Cache {
                                     kvStore = Map.fromList kvs
                                   , randomGen = mkStdGen seed
                                   , maxCapacity = size
                                   , auxKvStore = Map.empty
                                   }
                             Left _ -> Left "could not decode cache"
