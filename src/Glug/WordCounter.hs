module Glug.WordCounter (
  countWords
)
where

import qualified Data.Text as T
import qualified Data.Time.Clock as C
import Data.Char (isLetter)

import Glug.Types (WordCount (..), Subtitle (..))


-- | Collects the subtitles into individual words with some aggregate word
--   details.
countWords :: [Subtitle]  -- ^ The subtitles to analyze
              -> [WordCount] -- ^ An aggregate summary of all words
countWords = enumerateTree . addAllToTree Empty . wordsInSubtitles


addTime :: WordCount -> C.DiffTime -> WordCount
addTime wc t = wc { freq = freq', occurrences = occurrences' }
    where freq' = 1 + freq wc
          occurrences' = t : occurrences wc


wordsInSubtitles :: [Subtitle] -> [(T.Text, C.DiffTime)]
wordsInSubtitles []     = []
wordsInSubtitles (l:xl) = (includeTime . wordsInText $ dialogue l) ++ next
    where next = wordsInSubtitles xl
          includeTime = map (\t -> (t, timestamp l))


wordsInText :: T.Text -> [T.Text]
wordsInText t
      | T.null t       = []
      | T.null nextToc = []
      | otherwise      = nextToc : wordsInText rest
    where t'      = T.dropWhile isSplitToken t
          nextToc = T.toLower $ T.takeWhile (not . isSplitToken) t'
          rest    = T.drop (T.length nextToc) t'


isSplitToken :: Char -> Bool
isSplitToken c
    | c == '\'' = False
    | otherwise = not . isLetter $ c


data Tree = Tree WordCount Tree Tree
          | Empty


addToTree :: Tree -> (T.Text, C.DiffTime) -> Tree
addToTree Empty (s, t) = Tree (WordCount s 1 [t]) Empty Empty
addToTree (Tree wc left right) (s, t)
    | s == text wc      = Tree (addTime wc t) left right
    | s > text wc       = Tree wc (addToTree left (s, t)) right
    | otherwise         = Tree wc left (addToTree right (s, t))


addAllToTree :: Tree -> [(T.Text, C.DiffTime)] -> Tree
addAllToTree = foldr (flip addToTree)


enumerateTree :: Tree -> [WordCount]
enumerateTree t = enumerateTree' t []


enumerateTree' :: Tree -> [WordCount] -> [WordCount]
enumerateTree' Empty xs = xs
enumerateTree' (Tree wc left right) xs = enumerateTree' right $ enumerateTree' left $ wc : xs
