{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Glug.Net (
  realTlsGetUrl
) where

import qualified Data.ByteString.Lazy as BSL
import qualified Network.HTTP.Conduit as C

import Control.Concurrent (threadDelay)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Network.HTTP.Types (statusCode)

import Glug.Constants (useragent)


-- | Performs a network request for the resource at the given URL
realTlsGetUrl :: (MonadError String m, MonadIO m) =>
                  String
                  -- ^ the URL to request
                  -> m BSL.ByteString
                  -- ^ the response
realTlsGetUrl url = do
              req <- parseRequest url
              mgr <- liftIO $ C.newManager C.tlsManagerSettings
              let req' = req { C.requestHeaders = [("User-Agent", useragent)] }
              resp <- C.httpLbs req' mgr
              if (statusCode . C.responseStatus $ resp) == 409
                -- rate limited, pause and retry
                -- TODO make this a more sane strategy
                then do
                    liftIO $ threadDelay 100 -- pause 100ms
                    realTlsGetUrl url
                -- response available
                else return $ C.responseBody resp


parseRequest :: (MonadError String m) => String -> m C.Request
parseRequest url = case C.parseRequest url of
                     Nothing -> throwError "could not parse url"
                     Just req -> return req
