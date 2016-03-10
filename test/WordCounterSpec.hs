{-# LANGUAGE OverloadedStrings #-}

module WordCounterSpec (main, spec) where

import qualified Text.Subtitles.SRT as SRT
import Data.List (sortBy)
import Data.Int (Int32)
import Test.Hspec
import WordCounter

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "countWords" $ do
    it "find a word with frequency of 1" $ do
      let w = countWords [wordLine]
      w `shouldBe` [("foo", 1)]

    it "finds two lines with the same word and sums" $ do
      let w = countWords [wordLine, wordLine]
      w `shouldBe` [("foo", 2)]

    it "finds in multiple lines" $ do
      let w = countWords [wordLine, wordLine2, wordLine]
      (sortResults w) `shouldBe` [("foo", 2), ("bar", 1)]

    it "handles special characters" $ do
      let w = countWords [wordLineSpecial]
      w `shouldBe` [("ôèèüàç", 1)]


sortResults :: [(SRT.Text, Int32)] -> [(SRT.Text, Int32)]
sortResults = sortBy (\t1 t2 -> compare (snd t2) (snd t1))


wordLine :: SRT.Line
wordLine = SRT.Line 1
                    (SRT.Range
                        (SRT.Time 0 0 0 0)
                        (SRT.Time 0 1 1 32))
                      Nothing
                      "foo"

wordLine2 :: SRT.Line
wordLine2 = SRT.Line  1
                      (SRT.Range
                          (SRT.Time 0 0 0 0)
                          (SRT.Time 0 1 1 32))
                        Nothing
                        "bar"

wordLineSpecial :: SRT.Line
wordLineSpecial = SRT.Line 1
                    (SRT.Range
                        (SRT.Time 0 0 0 0)
                        (SRT.Time 0 1 1 32))
                      Nothing
                      "ôèèüàç"
