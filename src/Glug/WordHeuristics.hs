module Glug.WordHeuristics (
  bestCandidates
)
where


import qualified Data.Text as T
import qualified Glug.Types as WC (WordCount (..))
import qualified Glug.WordTrie as WT (commonWords, containsStr)
import Data.Int (Int32)
import Data.List (sort)

import Glug.Types (WordRank (..))


-- | Gets the best candidates from a list of words, sorted best first
bestCandidates :: [WC.WordCount] -- ^ The words to analyze
                  -> (Integer, Integer) -- ^ The accepable range of occurances, (min, max)
                  -> [WordRank] -- ^ The best candidate words
bestCandidates wcs range = reverse . sort . addIsCommon . addTimeGap . addSyllable . toWr $ dropWordsByFrequency wcs range
  where toWr wcs_ = map (flip WordRank $ 0) wcs_


dropWordsByFrequency :: [WC.WordCount] -> (Integer, Integer) -> [WC.WordCount]
dropWordsByFrequency wcs (min_, max_) = filter (between . toInteger . WC.freq) wcs
    where between x = x >= min_ && x <= max_


addIsCommon :: [WordRank] -> [WordRank]
addIsCommon = flip addToHeuristic $ \wc -> if isCommon $ WC.text wc then 0 else 4
    where isCommon = WT.containsStr WT.commonWords . T.unpack


addSyllable :: [WordRank] -> [WordRank]
addSyllable = flip addToHeuristic $ fromIntegral . (div 3) . T.length . WC.text


addTimeGap :: [WordRank] -> [WordRank]
addTimeGap wrs = flip addToHeuristic (timegap) wrs
    where timegap wc = round $ (mingap wc) / (expected wc) * 10
          expected = ((toRational maxtime) /) . fromIntegral . WC.freq
          mingap = minimum . difflist . map (toRational) . WC.occurances
          difflist [] = []
          difflist (_:[]) = []
          difflist (x:y:xs) = (y - x) : difflist (y:xs)
          maxtime = maximum . concat . map (WC.occurances . wordcount) $ wrs


addToHeuristic :: [WordRank] -> (WC.WordCount -> Int32) -> [WordRank]
addToHeuristic wrs f = map (bumpHeu) wrs
    where bumpHeu wr = wr { heuristic = (heuristic wr + (f $ wordcount wr)) }
