{-# LANGUAGE OverloadedStrings #-}

module CacheSpec (main, spec) where

import qualified Data.ByteString.Lazy as BSL

import Test.Hspec

import Data.Either (isLeft)

import Glug.Cache
import Glug.Monad

main :: IO ()
main = hspec spec


spec :: Spec
spec = do
  describe "tlsGetUrl" $ do
    it "returns cached responses" $ do
      result <- execMonadGlugIO singletonCache $ tlsGetUrl "http://example.com/foo"
      (fst result) `shouldBe` (Right "examplecontent")
    it "can hold more than one thing" $ do
      (Right (a, b), _) <- execMonadGlugIO twoItemCache $ do
          v1 <- tlsGetUrl "http://example.com/foo"
          v2 <- tlsGetUrl "http://example.com/bar"
          return (v1, v2)
      a `shouldBe` "examplecontent"
      b `shouldBe` "comeonandslam"
  describe "serialize / deserialize Cache" $ do
    it "handles one item" $ do
      let (Right cache) = deserializeCache 100 . serializeCache $ singletonCache
      (Right res, _) <- execMonadGlugIO cache $ tlsGetUrl "http://example.com/foo"
      res `shouldBe` "examplecontent"
    it "handles two items" $ do
      let (Right cache) = deserializeCache 100 . serializeCache $ twoItemCache
      (Right (a, b), _) <- execMonadGlugIO cache $ do
          v1 <- tlsGetUrl "http://example.com/foo"
          v2 <- tlsGetUrl "http://example.com/bar"
          return (v1, v2)
      a `shouldBe` "examplecontent"
      b `shouldBe` "comeonandslam"
    it "handles deserializing junk" $ do
      let res = deserializeCache 100 "somerandomjunkthatisntacache"
      res `shouldSatisfy` (isLeft)


singletonCache :: Cache
singletonCache = insertHardCache "http://example.com/foo" "examplecontent" $ newCache 100


twoItemCache :: Cache
twoItemCache = insertHardCache "http://example.com/foo" "examplecontent" $
               insertHardCache "http://example.com/bar" "comeonandslam" $
               newCache 100
