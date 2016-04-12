module WordTrieSpec (main, spec) where

import Test.Hspec
import Glug.WordTrie

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "containsStr" $ do
    it "should recognize when strings are not present" $ do
      (containsStr makeTrie "foo") `shouldBe` False

    it "stores and retrieves strings" $ do
      (containsStr (addStr makeTrie "f") "f") `shouldBe` True

    it "does not accept prefixes" $ do
      (containsStr (addStr makeTrie "carpet") "car") `shouldBe` False

    it "rejects empty srings" $ do
      (containsStr (addStr makeTrie "carpet") "") `shouldBe` False


  describe "addStr" $ do
    it "can add multiple strings" $ do
      let trie = makeTrieWithWords ["a", "b"]
      (containsStr trie "a") `shouldBe` True
      (containsStr trie "b") `shouldBe` True
      (containsStr trie "c") `shouldBe` False

    it "can add multiple complicated strings" $ do
      let trie = makeTrieWithWords ["foobar", "barbaz"]
      (containsStr trie "foobar") `shouldBe` True
      (containsStr trie "barbaz") `shouldBe` True
      (containsStr trie "barber") `shouldBe` False

    it "can add prefixes" $ do
      let trie = makeTrieWithWords ["foobar", "foo"]
      (containsStr trie "foobar") `shouldBe` True
      (containsStr trie "foo") `shouldBe` True

    it "can add longer words" $ do
      let trie = makeTrieWithWords ["you", "your"]
      (containsStr trie "you") `shouldBe` True
      (containsStr trie "your") `shouldBe` True
