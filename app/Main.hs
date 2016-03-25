{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Text.Subtitles.SRT as SRT
import Data.Char (isLatin1)
import Data.List (sortOn)
import System.IO (hFlush, stdout)
import Text.Read (readEither)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe

import Downloader
import WordHeuristics

type MaybeIO = MaybeT IO


main :: IO (Maybe ())
main = runMaybeT $ do
    ttls <- MaybeT $ getCandidateTitles
    lnk <- MaybeT $ chooseTitle ttls
    subs <- MaybeT $ getSubs lnk
    return ()


getCandidateTitles :: IO (Maybe [(T.Text, T.Text, Integer)])
getCandidateTitles = do
    putStr "Enter movie title: " >> hFlush stdout
    mov <- getLine
    titles <- candidateTitles mov
    case titles of
        Left s -> do
                    putStrLn $ "Could not find title (" ++ s ++ ")"
                    return Nothing
        Right s -> do
                    if null s
                      then putStrLn "No titles found" >> return Nothing
                      else return $ Just s


chooseTitle :: [(T.Text, T.Text, Integer)] -> IO (Maybe T.Text)
chooseTitle [] = putStrLn "No titles to show." >> return Nothing
chooseTitle bs = do
    printTitles bs' 1
    putStr "Select Number ('n' for next): " >> hFlush stdout
    n <- getLine
    case n of
      "n" -> chooseTitle $ drop 5 bs
      x -> case readEither x :: Either String Integer of
              Left _ -> putStrLn "Could not understand input" >> chooseTitle bs
              Right i -> if 1 <= i && i <= (toInteger $ length bs')
                         then return . Just . getLnk $ bs' !! (fromIntegral i-1)
                         else putStrLn "Number out of range" >> chooseTitle bs
    where bs' = take 5 bs
          printTitle (_, t, n) i = T.putStrLn . sanitizeForPrint $ T.concat ["[", T.pack $ show i, "] ", t, " - ", T.pack $ show n]
          printTitles [] _ = return ()
          printTitles (x:xs) i = (printTitle x i) >> (printTitles xs (i+1))
          getLnk (l, _, _) = l


getSubs :: T.Text -> IO (Maybe SRT.Subtitles)
getSubs s = do
    subs <- getSubtitles $ T.unpack s
    case subs of
      Left s -> (putStrLn $ "Could not find .srt: " ++ s) >> return Nothing
      Right s -> return . Just $ s


sanitizeForPrint :: T.Text -> T.Text
sanitizeForPrint = T.map conv
  where conv c = if isLatin1 c then c else '?'
