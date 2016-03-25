module WordCounter (
  countWords
, WordCount (..)
)
where

import qualified Data.Text as T
import qualified Text.Subtitles.SRT as SRT
import qualified Data.Time.Clock as C
import Data.Char (isLetter)
import Data.Int (Int32)


data WordCount = WordCount { text :: T.Text
                           , freq :: Int32
                           , occurances :: [C.DiffTime]
                           } deriving (Show, Eq)


addTime :: WordCount -> C.DiffTime -> WordCount
addTime wc t = wc { freq = freq', occurances = occurances' }
    where freq' = 1 + freq wc
          occurances' = t : (occurances wc)


countWords :: SRT.Subtitles -> [WordCount]
countWords = enumerateTree . (addAllToTree Empty) . wordsInSubtitles


wordsInSubtitles :: SRT.Subtitles -> [(T.Text, C.DiffTime)]
wordsInSubtitles []     = []
wordsInSubtitles (l:xl) = (includeTime . wordsInText $ SRT.dialog l) ++ next
    where next = wordsInSubtitles xl
          time = toDiffTime $ SRT.from . SRT.range $ l
          includeTime = map (\t -> (t, time))


wordsInText :: T.Text -> [T.Text]
wordsInText t
      | T.null t       = []
      | T.null nextToc = []
      | otherwise      = nextToc : (wordsInText rest)
    where t'      = T.dropWhile (isSplitToken) t
          nextToc = T.toLower $ T.takeWhile (not . isSplitToken) t'
          rest    = T.drop (T.length nextToc) t'


isSplitToken :: Char -> Bool
isSplitToken = not . isLetter


data Tree = Tree WordCount Tree Tree
          | Empty


addToTree :: Tree -> (T.Text, C.DiffTime) -> Tree
addToTree Empty (s, t) = Tree (WordCount s 1 [t]) Empty Empty
addToTree (Tree wc left right) (s, t)
    | s == (text wc)      = Tree (addTime wc t) left right
    | s > (text wc)       = Tree wc (addToTree left (s, t)) right
    | otherwise           = Tree wc left (addToTree right (s, t))


addAllToTree :: Tree -> [(T.Text, C.DiffTime)] -> Tree
addAllToTree tre txts = foldr (flip addToTree) tre txts


enumerateTree :: Tree -> [WordCount]
enumerateTree t = enumerateTree' t []


enumerateTree' :: Tree -> [WordCount] -> [WordCount]
enumerateTree' Empty xs = xs
enumerateTree' (Tree wc left right) xs = enumerateTree' right $ enumerateTree' left $ wc : xs


toDiffTime :: SRT.Time -> C.DiffTime
toDiffTime t = C.secondsToDiffTime . toInteger $
      (SRT.hour t) * 60 * 60 + (SRT.minutes t) * 60 + (SRT.seconds t)
