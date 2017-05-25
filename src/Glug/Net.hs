{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Glug.Net (
  realTlsGetUrl
) where

import qualified Data.ByteString.Lazy as BSL
import qualified Network.HTTP.Conduit as C

import Control.Monad.Except (MonadError)
import Control.Monad.Writer (MonadWriter)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Glug.Monad (logM, hoistMaybe)
import Glug.Constants (useragent)


-- | Performs a network request for the resource at the given URL
realTlsGetUrl :: (MonadError String m, MonadIO m, MonadWriter [String] m) =>
                  String
                  -- ^ the URL to request
                  -> m BSL.ByteString
                  -- ^ the response
realTlsGetUrl url = do
    logM "realTlsGetUrl" $ "getting URL " ++ url
    req <- hoistMaybe "could not parse url" . C.parseRequest $ url
    mgr <- liftIO $ C.newManager C.tlsManagerSettings
    let req' = req { C.requestHeaders = [("User-Agent", useragent)] }
    C.responseBody <$> C.httpLbs req' mgr
