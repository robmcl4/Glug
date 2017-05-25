{-# LANGUAGE OverloadedStrings #-}

module Glug.Constants (
  version
, useragent
, defaultLRUSize
) where

import qualified Paths_glug as P
import qualified Data.ByteString as BS (ByteString, concat)
import qualified Data.Text as T
import qualified Data.Text.Encoding as ENC

import Data.Version (showVersion)

version :: T.Text
version = T.pack . showVersion $ P.version

useragent :: BS.ByteString
useragent = BS.concat ["haskell/glug " :: BS.ByteString, ENC.encodeUtf8 version]

defaultLRUSize :: Integer
defaultLRUSize = 256
