{-# LANGUAGE OverloadedStrings #-}

module CommonWordsSpec (main, spec) where

import Test.Hspec
import Glug.CommonWords

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "commonality" $ do
    it "should recognize when strings are not present" $ do
      (commonality "whoaimnotawordthere") `shouldBe` 0

    it "maxes out at 1000 commonality" $ do
      (commonality "the") `shouldBe` 1000

    it "rejects empty srings" $ do
      (commonality "") `shouldBe` 0
