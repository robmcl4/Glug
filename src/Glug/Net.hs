{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Glug.Net (
  realTlsGetUrl
) where

import qualified Data.ByteString.Lazy as BSL
import qualified Network.HTTP.Conduit as C

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Glug.Constants (useragent)


-- | Performs a network request for the resource at the given URL
realTlsGetUrl :: (MonadError String m, MonadIO m) =>
                  String
                  -- ^ the URL to request
                  -> m BSL.ByteString
                  -- ^ the response
realTlsGetUrl url = case C.parseRequest url of
                      Nothing -> throwError "could not parse url"
                      Just req -> do
                        mgr <- liftIO $ C.newManager C.tlsManagerSettings
                        let req' = req { C.requestHeaders = [("User-Agent", useragent)] }
                        C.responseBody <$> C.httpLbs req' mgr
