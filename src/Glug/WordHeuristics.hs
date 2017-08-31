module Glug.WordHeuristics (
  bestCandidates
)
where


import qualified Data.Text as T
import qualified Glug.Types as WC (WordCount (..))
import Data.Int (Int32)
import Data.List (sortBy)

import Glug.Types (WordRank (..))
import Glug.CommonWords (commonality)


-- | Gets the best candidates from a list of words, sorted best first
bestCandidates :: [WC.WordCount] -- ^ The words to analyze
                  -> (Integer, Integer) -- ^ The accepable range of occurrences, (min, max)
                  -> [WordRank] -- ^ The best candidate words
bestCandidates wcs range = sortBy (flip compare) . addIsCommon . addTimeGap . addSyllable . toWr $ dropWordsByFrequency wcs range
  where toWr = map (flip WordRank 0)


dropWordsByFrequency :: [WC.WordCount] -> (Integer, Integer) -> [WC.WordCount]
dropWordsByFrequency wcs (min_, max_) = filter (between . toInteger . WC.freq) wcs
    where between x = x >= min_ && x <= max_


-- NOTE higher heuristic is better fitness


addIsCommon :: [WordRank] -> [WordRank]
addIsCommon = addToHeuristic $ \wc -> fromIntegral $ 5 - commonality (WC.text wc) * 5 `div` 1000


addSyllable :: [WordRank] -> [WordRank]
addSyllable = addToHeuristic $ \wc -> fromIntegral $ T.length (WC.text wc) `div` 3


addTimeGap :: [WordRank] -> [WordRank]
addTimeGap wrs = addToHeuristic timegap wrs
    where weight = 5
          timegap wc = round $ mingap wc / expected wc * weight
          expected wc = maxtime / fromIntegral (WC.freq wc)
          -- TODO make this safe for words that appear only once (?)
          mingap = toRational . minimum . difflist . WC.occurrences
          difflist [] = []
          difflist [_] = []
          difflist (x:y:xs) = (y - x) : difflist (y:xs)
          maxtime = toRational . maximum . concatMap (WC.occurrences . wordcount) $ wrs


addToHeuristic :: (WC.WordCount -> Int32) -> [WordRank] -> [WordRank]
addToHeuristic f = map bumpHeu
    where bumpHeu wr = wr { heuristic = heuristic wr + f (wordcount wr) }
