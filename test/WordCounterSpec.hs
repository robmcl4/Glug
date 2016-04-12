{-# LANGUAGE OverloadedStrings #-}

module WordCounterSpec (main, spec) where

import qualified Text.Subtitles.SRT as SRT
import qualified Data.Time.Clock as C
import Data.List (sortOn)
import Data.Int (Int32)
import Test.Hspec
import Glug.WordCounter

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "countWords" $ do
    it "find a word with frequency of 1" $ do
      let w = countWords [wordLine]
      w `shouldBe` [WordCount "foo" 1 [C.secondsToDiffTime 0]]

    it "finds two lines with the same word and sums" $ do
      let w = countWords [wordLine, wordLine]
      w `shouldBe` [(WordCount "foo" 2 [(C.secondsToDiffTime 0), (C.secondsToDiffTime 0)])]

    it "finds in multiple lines" $ do
      let w = countWords [wordLine, wordLine2, wordLine]
      (sortResults w) `shouldBe` [(WordCount "foo" 2 [(C.secondsToDiffTime 0), (C.secondsToDiffTime 0)]), (WordCount "bar" 1 [C.secondsToDiffTime 61])]

    it "handles special characters" $ do
      let w = countWords [wordLineSpecial]
      w `shouldBe` [WordCount "ôèèüàç" 1 [C.secondsToDiffTime 0]]

    it "groups words" $ do
      let w = countWords [wordLine, wordLine']
      w `shouldBe` [WordCount "foo" 2 [(C.secondsToDiffTime 0), (C.secondsToDiffTime 80)]]


sortResults :: [WordCount] -> [WordCount]
sortResults = sortOn (occurances)


wordLine :: SRT.Line
wordLine = SRT.Line 1
                    (SRT.Range
                        (SRT.Time 0 0 0 0)
                        (SRT.Time 0 1 1 32))
                      Nothing
                      "foo"

wordLine' :: SRT.Line
wordLine' = SRT.Line 1
                    (SRT.Range
                        (SRT.Time 0 1 20 0)
                        (SRT.Time 0 1 25 0))
                      Nothing
                      "foo"


wordLine2 :: SRT.Line
wordLine2 = SRT.Line  1
                      (SRT.Range
                          (SRT.Time 0 1 1 32)
                          (SRT.Time 0 2 2 64))
                        Nothing
                        "bar"

wordLineSpecial :: SRT.Line
wordLineSpecial = SRT.Line 1
                    (SRT.Range
                        (SRT.Time 0 0 0 0)
                        (SRT.Time 0 1 1 32))
                      Nothing
                      "ôèèüàç"
