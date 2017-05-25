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
      cache <- singletonCache
      result <- execMonadGlugIOWithCache cache $ tlsGetUrl "http://example.com/foo"
      (fst result) `shouldBe` (Right "examplecontent")

  describe "serialize / load cache" $ do
    it "serializes and loads a single item" $ do
      cache <- singletonCache
      result <- execMonadGlugIOWithCache cache $ do
          serializeCache >>= loadCache
          tlsGetUrl "http://example.com/foo"
      (fst result) `shouldBe` (Right "examplecontent")
    it "serializes and loads multiple items" $ do
      cache <- twoItemCache
      lru <- readMVar cache
      result <- execMonadGlugIOWithCache cache $ do
          serializeCache >>= loadCache
          r1 <- tlsGetUrl "http://example.com/foo"
          r2 <- tlsGetUrl "http://example.com/bar"
          return (r1, r2)
      (fst result) `shouldBe` (Right ("examplecontent", "comeonandslam"))
    it "can load an old serialized cache" $ do
      cache1 <- singletonCache
      cache2 <- twoItemCache
      ((Right bsl), _) <- execMonadGlugIOWithCache cache2 serializeCache
      ((Right result), _) <- execMonadGlugIOWithCache cache1 $ do
          loadCache bsl
          tlsGetUrl "http://example.com/bar"
      result `shouldBe` "comeonandslam"

singletonCache :: IO Cache
singletonCache = do
    cache <- newCache 1
    -- insert into cache "http://example.com/foo" -> "examplecontent"
    lru <- takeMVar cache
    let lru' = LRU.insert "http://example.com/foo" "examplecontent" lru
    putMVar cache lru'
    return cache


twoItemCache :: IO Cache
twoItemCache = do
    cache <- newCache 1
    swapMVar cache $ LRU.fromList (Just 100)
                                 [("http://example.com/foo", "examplecontent")
                                 ,("http://example.com/bar", "comeonandslam")]
    return cache
