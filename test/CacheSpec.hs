{-# LANGUAGE OverloadedStrings #-}

module CacheSpec (main, spec) where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Cache.LRU as LRU

import Test.Hspec

import Control.Concurrent.MVar
import Glug.Cache
import Glug.Types
import Glug.Monad

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "tlsGetUrl" $ do
    it "returns cached responses" $ do
      cache <- newCache 1
      -- insert into cache "http://example.com/foo" -> "examplecontent"
      lru <- takeMVar cache
      let lru' = LRU.insert "http://example.com/foo" "examplecontent" lru
      putMVar cache lru'
      result <- execMonadGlugIOWithCache cache $ tlsGetUrl "http://example.com/foo"
      (fst result) `shouldBe` (Right "examplecontent")
