module WordCounter (
  countWords
)
where

import qualified Data.Text as T
import qualified Text.Subtitles.SRT as SRT
import qualified Text.Subtitles.SRT.Datatypes as SRT
import Data.Char (isLetter)
import Data.List (elemIndex, foldr)
import Data.Int (Int32)


countWords :: SRT.Subtitles -> [(T.Text, Int32)]
countWords = enumerateTree . (addAllToTree Empty) . (map T.toLower) . wordsInSubtitles


wordsInSubtitles :: SRT.Subtitles -> [T.Text]
wordsInSubtitles []     = []
wordsInSubtitles (l:xl) = (wordsInText $ SRT.dialog l) ++ (wordsInSubtitles xl)


wordsInText :: T.Text -> [T.Text]
wordsInText t
      | T.null t       = []
      | T.null nextToc = []
      | otherwise      = nextToc : (wordsInText rest)
    where t'      = T.dropWhile (isSplitToken) t
          nextToc = T.takeWhile (not . isSplitToken) t'
          rest    = T.drop (T.length nextToc) t'


isSplitToken :: Char -> Bool
isSplitToken = not . isLetter


data Tree = Tree T.Text Int32 Tree Tree
          | Empty


addToTree :: Tree -> T.Text -> Tree
addToTree Empty s = Tree s 1 Empty Empty
addToTree (Tree ts i left right) s
    | s == ts      = Tree ts (i+1) left right
    | s < ts       = Tree ts i (addToTree left s) right
    | s > ts       = Tree ts i left (addToTree right s)


addAllToTree :: Tree -> [T.Text] -> Tree
addAllToTree tre txts = foldr (flip addToTree) Empty txts


enumerateTree :: Tree -> [(T.Text, Int32)]
enumerateTree t = enumerateTree' t []


enumerateTree' :: Tree -> [(T.Text, Int32)] -> [(T.Text, Int32)]
enumerateTree' Empty xs = xs
enumerateTree' (Tree txt i left right) xs = enumerateTree' right $ enumerateTree' left $ (txt, i) : xs
