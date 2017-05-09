{-# LANGUAGE OverloadedStrings #-}

module WordCounterSpec (main, spec) where

import qualified Text.Subtitles.SRT as SRT
import qualified Data.Time.Clock as C
import Data.List (sortOn)
import Data.Int (Int32)
import Test.Hspec
import Glug.WordCounter
import Glug.Types (WordCount (..), Subtitle (..))


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

    it "doesn't split contractions" $ do
      let w = countWords [wordLine3]
      w `shouldBe` [(WordCount "isn't" 1 [C.secondsToDiffTime 0])]


sortResults :: [WordCount] -> [WordCount]
sortResults = sortOn (occurrences)


wordLine :: Subtitle
wordLine = Subtitle { dialogue = "foo"
                    , timestamp = C.secondsToDiffTime 0 }

wordLine' :: Subtitle
wordLine' = Subtitle { dialogue = "foo"
                    , timestamp = C.secondsToDiffTime 80 }


wordLine2 :: Subtitle
wordLine2 = Subtitle { dialogue = "bar"
                     , timestamp = C.secondsToDiffTime 61 }


wordLine3 :: Subtitle
wordLine3 = Subtitle { dialogue = "isn't"
                     , timestamp = C.secondsToDiffTime 0 }


wordLineSpecial :: Subtitle
wordLineSpecial = Subtitle { dialogue = "ôèèüàç"
                           , timestamp = C.secondsToDiffTime 0 }
