module API.CacheMaintenance (
  maintainCache
) where

import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Concurrent (forkIO)
import Control.Monad (forever)

import Glug (Cache, mergeCache)

-- | Performs cache maintenance duties in a separate thread.
--   Consumes from channel Chan and reduces into the Cache MVar.
maintainCache :: Chan Cache -> MVar Cache -> IO ()
maintainCache c m = (forkIO $ reduceCache c m) >>= \_ -> return ()


reduceCache :: Chan Cache -> MVar Cache -> IO ()
reduceCache chan mvar = forever $ do
    got <- readChan chan
    old <- takeMVar mvar
    putMVar mvar $ mergeCache old got
