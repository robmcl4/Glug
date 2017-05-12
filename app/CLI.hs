module CLI (
  main
) where

import qualified Data.Text.Lazy as T
import qualified Data.Text as TS
import Data.Char (isLatin1)
import System.IO (hFlush, stdout)
import Text.Read (readEither)
import Control.Monad (liftM, mzero, mplus, forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe

import Glug

type MaybeIO = MaybeT IO


main :: IO ()
main = (runMaybeT $ do
  ttls <- getCandidateTitles
  lnk <- chooseTitle ttls
  subs <- getSubs lnk
  range <- getRange
  liftIO $ printBest subs range) >> return ()


getCandidateTitles :: MaybeIO [(T.Text, T.Text, Integer)]
getCandidateTitles = do
    mov <- prompt "Enter movie title: "
    titles <- liftIO . fmap fst . execMonadGlugIO $ candidateTitles mov
    case titles of
        Left s -> putStrLn' $ "Could not find title (" ++ s ++ ")"
        Right s -> if null s
                      then putStrLn' "No titles found"
                      else return s


chooseTitle :: [(T.Text, T.Text, Integer)] -> MaybeIO T.Text
chooseTitle [] = putStrLn' "No titles to show."
chooseTitle bs = do
    liftIO $ printTitles bs'
    n <- prompt "Select Number ('n' for next): "
    case n of
      "n" -> chooseTitle rest
      x   -> case readEither x :: Either String Integer of
              Left _ -> liftIO (putStrLn "Could not understand input") >> chooseTitle bs
              Right i -> if 1 <= i && i <= (toInteger $ length bs')
                         then return . getLnk $ bs' !! (fromIntegral i-1)
                         else liftIO (putStrLn "Number out of range") >> chooseTitle bs
    where bs' = take 5 bs
          rest = drop 5 bs
          printTitle (_, t, n) i = putStrLn $ "[" ++ show i ++ "] " ++ (T.unpack . sanitizeForPrint $ t) ++ " - " ++ show n
          printTitles xs = forM_ (zip xs ([1..] :: [Integer])) (uncurry printTitle)
          getLnk (l, _, _) = l


getSubs :: T.Text -> MaybeIO [Subtitle]
getSubs slnk = do
    subs <- liftIO $ fmap fst . execMonadGlugIO . getSubtitles $ T.unpack slnk
    case liftM (subtitles) subs of
      Left s -> putStrLn' $ "Could not find .srt: " ++ s
      Right s -> return s


getRange :: MaybeIO (Integer, Integer)
getRange = do
    n1 <- promptI "minimum occurrences: "
    n2 <- promptI "maximum occurrences: "
    return (n1, n2)


printBest :: [Subtitle] -> (Integer, Integer) -> IO ()
printBest s rng = printWrs best
  where best = (take 8) $ bestCandidates wcs rng
        wcs = countWords s
        printWrs xs = forM_ xs printWr
        printWr = putStrLn . wrtos
        wrtos wr = wctos . wordcount $ wr
        wctos wc = (TS.unpack . text $ wc) ++ ": " ++ (show . freq $ wc) ++ "\n" ++ occViz wc
        occViz wc = "[" ++ (dashes . map (dashLoc) $ occurrences wc) ++ "]"
        dashes xs = addDash xs ndashes
        addDash [] _ = ""
        addDash _  0 = ""
        addDash occ n = (if (ndashes - n) `elem` occ then 'X' else '-') : (addDash occ (n-1))
        dashLoc dt = round $ toRational (dt / maxtime) * toRational ndashes
        ndashes = 80 :: Integer
        maxtime = maximum . concat . map (occurrences) $ wcs


-- ---------------------------- Utilities ----------------------------------- --

promptI :: String -> MaybeIO Integer
promptI s = do
    got <- prompt s
    case readEither got :: Either String Integer of
        Left _ -> putStrLn' "Could not understand input" `mplus` promptI s
        Right i -> return i


prompt :: String -> MaybeIO String
prompt s = MaybeT $ putStr s >> hFlush stdout >> liftM Just getLine


putStrLn' :: String -> MaybeIO a
putStrLn' s = liftIO $ putStrLn s >> mzero


sanitizeForPrint :: T.Text -> T.Text
sanitizeForPrint = T.map conv
  where conv c = if isLatin1 c then c else '?'
