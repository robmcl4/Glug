{-# LANGUAGE OverloadedStrings #-}

module Glug.SubsceneDownloader (
  candidateTitles
, getSubtitles
)
where


import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Text.EditDistance as ED
import qualified Text.HTML.TagSoup as TS

import Control.Monad.Except
import Data.Char (isSpace)
import Data.List (group, sort, sortOn)
import Network.HTTP.Base (urlEncode)
import Text.Read (readEither)

import Glug.Monad (MonadGlugIO (..), hoistEither)
import Glug.SrtExtract (parseSrtFromZip)
import Glug.Types (MovieSubtitles (..))
import Glug.Cache (tlsGetUrl)

searchurl :: String
subscenebase :: String
searchurl = "https://subscene.com/subtitles/title?q="
subscenebase = "https://subscene.com"


-- | Gets the subtitles from a movie
getSubtitles :: String -- ^ The path to the subtitle listing on subscene
                -> MonadGlugIO String MovieSubtitles -- ^ represents an error
                                                     -- message or movie subtitles
getSubtitles s = do
    soup <- getSoup $ subscenebase ++ s
    cands <- hoistEither $ getSubLinks soup
    id_ <- hoistEither $ getImdbUrl soup >>= extractId >>= pad >>= Right . T.append "tt"
    subs <- getSub cands
    return MovieSubtitles { imdbid = id_, subtitles = subs }
  where getSub [] = throwError "No subtitles found"
        getSub (x:xs) = (subAt . T.unpack $ x) `catchError` (\_ -> getSub xs)
        subAt subpath = do
            soup <- getSoup $ subscenebase ++ subpath
            downLink <- hoistEither $ getDownloadLink soup
            subs <- tlsGetUrl (subscenebase ++ T.unpack downLink)
            parseSrtFromZip subs
        extractId t = fromMaybe (T.stripPrefix "http://www.imdb.com/title/tt" t) "did not find valid imdb id"
        pad = Right . T.justifyRight 7 '0'


-- | Gets candidate titles from a movie after searching by title
candidateTitles :: String
                   -- ^ The movie title being searched
                   -> MonadGlugIO String [(T.Text, T.Text, Integer)]
                   -- ^ represents either an error message or a list of
                   --   (href, title, no. of subs)
candidateTitles s = do
    soup <- getSoup $ searchurl ++ quotePlus s
    titles <- hoistEither $ getTitles soup
    return . sortOn (\(_, t, _) -> editDist (T.unpack t)) . dedup $ titles
  where dedup = map head . group . sort
        editDist = ED.levenshteinDistance ED.defaultEditCosts s


getSoup :: String -> MonadGlugIO String [TS.Tag T.Text]
getSoup url = do
    bs <- tlsGetUrl url
    txt <- hoistEither . eitherShow . T.decodeUtf8' $ bs
    return . TS.parseTags $ txt

-- ----------------------------- Soup Handling ------------------------------ --

getDownloadLink :: [TS.Tag T.Text] -> Either String T.Text
getDownloadLink [] = Left "no download link found"
getDownloadLink (TS.TagOpen name attrs:xs)
    | name == "a" && ("id", "downloadButton") `elem` attrs
                = eitherAttr attrs "href"
    | otherwise = getDownloadLink xs
getDownloadLink (_:xs) = getDownloadLink xs


-- gets subtitle links, prioritized by non-HoH enhanced first
getSubLinks :: [TS.Tag T.Text] -> Either String [T.Text]
getSubLinks ts = prioritize <$> getSubLinks' ts
  where -- construct prioritization using an accumulator to collect non-priority
        -- on descent, and appending prioritized to head on recursive ascent
        prioritize xs = prioritize' xs []
        prioritize' [] acc = acc
        prioritize' ((x, False):xs) acc = x : prioritize' xs acc
        prioritize' ((x, True):xs)  acc = prioritize' xs (x:acc)


-- gets subtitle links in the form of (url, isHardOfHearing)
getSubLinks' :: [TS.Tag T.Text] -> Either String [(T.Text, Bool)]
getSubLinks' [] = Right []
getSubLinks' (TS.TagOpen name1 attrs1: -- <a {attrs}>
             TS.TagText _: -- whitespace
             TS.TagOpen name2 attrs2: -- <span {attrs}>
             TS.TagText engl: -- language
             ts)
    | name1 == "a" && name2 == "span" && removeSpaces engl == "English" && attrs2 == [("class", "l r positive-icon")]
                = do
                    hrf <- eitherAttr attrs1 "href"
                    rest <- getSubLinks' ts
                    return ((hrf, findIsHoHEnhanced ts):rest)
    | otherwise = getSubLinks' (TS.TagOpen name2 attrs2:(TS.TagText engl:ts))
  where removeSpaces = T.filter (not . isSpace)
        findIsHoHEnhanced [] = False
        findIsHoHEnhanced (TS.TagOpen name1' attrs1':ts')
            | name1' == "td" && attrs1' == [("class", "a40")] = False
            | name1' == "td" && attrs1' == [("class", "a41")] = True
            | otherwise = findIsHoHEnhanced ts'
        findIsHoHEnhanced (_:ts') = findIsHoHEnhanced ts'
getSubLinks' (_:ts) = getSubLinks' ts


getTitles :: [TS.Tag T.Text] -> Either String [(T.Text, T.Text, Integer)]
getTitles [] = Right []
getTitles (TS.TagOpen name attrs: xs)
    | name == "div" && attrs == [("class", "title")] = do
        (href, title, rest) <- findHrefTitle xs
        (i, rest') <- findCount rest
        next <- getTitles rest'
        Right $ (href, title, i) : next
  where findHrefTitle [] = Left "No title found"
        findHrefTitle (TS.TagOpen name' attrs':
                       TS.TagText title: ts)
            | name' == "a" = do
                href <- eitherAttr attrs' "href"
                Right (href, title, ts)
            | otherwise     = findHrefTitle ts
        findHrefTitle (_:ts) = findHrefTitle ts
        findCount [] = Left "No count found"
        findCount (TS.TagOpen name' attrs':TS.TagText txt:ts)
            | name' == "div" && attrs' == [("class", "subtle count")]
                        = readEither txt' >>= \i -> return (i, ts)
            | otherwise = findCount ts
          where txt' = takeWhile (/= ' ') . T.unpack $ txt
        findCount (_:ts) = findCount ts
getTitles (_:xs) = getTitles xs


getImdbUrl :: [TS.Tag T.Text] -> Either String T.Text
getImdbUrl [] = Left "No IMDb Id Found"
getImdbUrl (TS.TagOpen name attrs: _)
    | name == "a" && ("class", "imdb") `elem` attrs = eitherAttr attrs "href"
getImdbUrl (_:xs) = getImdbUrl xs


-- -------------------------------- Utilities ------------------------------- --

eitherShow :: Show a => Either a b -> Either String b
eitherShow (Left x) = Left $ show x
eitherShow (Right x) = Right x


eitherAttr :: [(T.Text, T.Text)] -> T.Text
                  -> Either String T.Text
eitherAttr [] s = Left $ "could not find attr " ++ T.unpack s
eitherAttr ((k, v):xs) s
    | k == s    = Right v
    | otherwise = eitherAttr xs s


fromMaybe :: Maybe b -> String -> Either String b
fromMaybe Nothing s = Left s
fromMaybe (Just x) _ = Right x


quotePlus :: String -> String
quotePlus = replace20 . urlEncode


replace20 :: String -> String
replace20 [] = []
replace20 [x] = [x]
replace20 [x, y] = [x, y]
replace20 (x:y:z:zs)
  | [x, y, z] == "%20" = '+' : replace20 zs
  | otherwise          = x : replace20 (y:z:zs)
