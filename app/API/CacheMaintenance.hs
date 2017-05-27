module API.CacheMaintenance (
  spinupCache
) where

import qualified Data.ByteString.Lazy as BSL

import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (tryJust)
import Control.Monad (forever, guard)
import System.IO (withFile, IOMode (ReadMode, WriteMode))
import System.IO.Error (isDoesNotExistError)

import Glug (Cache, newCache, mergeCache, serializeCache, deserializeCache)


cacheSize :: Int
cacheSize = 256


-- | Creates a cache and accessors.
--   Performs cache maintenance duties in a separate thread.
--   Consumes from channel Chan and reduces into the Cache MVar.
--   Regularly writes the cache out to a file.
spinupCache :: IO (MVar Cache, Chan Cache)
spinupCache = do
    m <- inflateCache >>= newMVar
    c <- newChan
    _ <- forkIO $ reduceCache c m
    _ <- forkIO $ persistCache m
    return (m, c)


inflateCache :: IO (Cache)
inflateCache = do
    r <- tryJust (guard . isDoesNotExistError) $ _inflateCache
    case r of
      Left _ -> return $ newCache cacheSize
      Right x -> return x


_inflateCache :: IO (Cache)
_inflateCache = do
    withFile "cache.dat" ReadMode $ \f -> do
      bsl <- BSL.hGetContents f
      case deserializeCache cacheSize bsl of
          Right oldCache -> do
              putStrLn "Inflated from serialized cache"
              return oldCache
          Left _ -> do
              putStrLn "Faild to inflate from cache"
              return $ newCache cacheSize


persistCache :: MVar Cache -> IO ()
persistCache mvar = forever $ do
    threadDelay $ 60 * 1000 * 1000
    got <- readMVar mvar
    withFile "cache.dat" WriteMode $ \f ->
        BSL.hPut f . serializeCache $ got
    putStrLn "Persisted Cache"


reduceCache :: Chan Cache -> MVar Cache -> IO ()
reduceCache chan mvar = forever $ do
    got <- readChan chan
    old <- takeMVar mvar
    putMVar mvar $ mergeCache old got
