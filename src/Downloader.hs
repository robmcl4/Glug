{-# LANGUAGE OverloadedStrings #-}

module Downloader (
  candidateTitles
, getSubtitles
)
where


import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B (w2c, c2w)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Network.URI as URI
import qualified Network.HTTP as HTTP
import qualified Network.HTTP.Base as HTB
import qualified Network.HTTP.Headers as HTH
import qualified Text.EditDistance as ED
import qualified Text.HTML.TagSoup as TS
import qualified Text.Subtitles.SRT as SRT

import Data.Char (isSpace)
import Data.List (find, group, sort, sortOn)
import Text.Read (readEither)
import Network.BufferType (BufferType)
import Network.Stream (Result)
import Control.Monad (mplus)
import Control.Monad.Except

import SrtExtract (parseSrtFromZip)

searchurl = "http://subscene.com/subtitles/title?q="
subscenebase = "http://subscene.com"
useragent = "haskell/glug"


getSubtitles :: String -> IO (Either String SRT.Subtitles)
getSubtitles s = runExceptT $ do
    cands <- candidateSubtitles s
    getSub cands
  where getSub [] = throwError "No subtitles found"
        getSub (x:xs) = (subAt $ T.unpack x) `mplus` (getSub xs)
        subAt s = do
            soup <- getSoup $ subscenebase ++ s
            downLink <- liftEither $ getDownloadLink soup
            subs <- makeGet (subscenebase ++ (T.unpack downLink))
            liftEither $ parseSrtFromZip subs


candidateSubtitles :: String -> ExceptT String IO [T.Text]
candidateSubtitles s = do
    soup <- getSoup $ subscenebase ++ s
    liftEither $ getSubLinks soup


candidateTitles :: String -> IO (Either String [(T.Text, T.Text, Integer)])
candidateTitles s = runExceptT $ do
    soup <- getSoup $ searchurl ++ (quote_plus s)
    titles <- liftEither $ getTitles soup
    return . sortOn (\(_, t, _) -> editDist (T.unpack t)) . dedup $ titles
  where dedup = map (head) . group . sort
        editDist = ED.levenshteinDistance ED.defaultEditCosts s


getSoup :: String -> ExceptT String IO ([TS.Tag T.Text])
getSoup s = do
    bs <- makeGet s
    txt <- ExceptT . return . eitherShow . T.decodeUtf8' $ bs
    return . TS.parseTags $ txt


makeGet :: String -> ExceptT String IO BSL.ByteString
makeGet url = liftEither uri >>= makeGetReq . req
    where uri = fromMaybe (URI.parseURI url) "Could not parse URI"
          req u = HTH.replaceHeader HTH.HdrUserAgent useragent $ HTB.mkRequest HTB.GET u


makeGetReq :: HTB.Request BSL.ByteString -> ExceptT String IO BSL.ByteString
makeGetReq r = do
    resp <- (withExceptT show) . ExceptT $ HTTP.simpleHTTP r
    case HTB.rspCode resp of
      (2, _, _) -> return $ HTB.rspBody resp
      (3, _, _) -> (liftEither $ findHeader resp HTH.HdrLocation) >>= makeGet
      _         -> throwError "bad status code"

-- ----------------------------- Soup Handling ------------------------------ --

getDownloadLink :: [TS.Tag T.Text] -> Either String T.Text
getDownloadLink [] = Left "no download link found"
getDownloadLink ((TS.TagOpen name attrs):xs)
    | name == "a" && ("id", "downloadButton") `elem` attrs
                = eitherAttr attrs "href"
    | otherwise = getDownloadLink xs
getDownloadLink (x:xs) = getDownloadLink xs


getSubLinks :: [TS.Tag T.Text] -> Either String [T.Text]
getSubLinks [] = Right []
getSubLinks ((TS.TagOpen name1 attrs1):
             (TS.TagText _):
             (TS.TagOpen name2 attrs2):
             (TS.TagText engl):
             ts)
    | name1 == "a" && name2 == "span" && (removeSpaces engl) == "English" && attrs2 == [("class", "l r positive-icon")]
                = do
                    hrf <- eitherAttr attrs1 "href"
                    rest <- getSubLinks ts
                    return ((hrf):rest)
    | otherwise = getSubLinks ((TS.TagOpen name2 attrs2):((TS.TagText engl):ts))
  where removeSpaces = T.filter (not . isSpace)
getSubLinks (t:ts) = getSubLinks ts


getTitles :: [TS.Tag T.Text] -> Either String [(T.Text, T.Text, Integer)]
getTitles [] = Right []
getTitles ((TS.TagOpen name attrs): xs)
    | name == "div" && attrs == [("class", "title")] = do
        (href, title, rest) <- findHrefTitle xs
        (i, rest') <- findCount rest
        next <- getTitles rest'
        Right $ (href, title, i) : next
    | otherwise = getTitles xs
  where findHrefTitle [] = Left "No title found"
        findHrefTitle ((TS.TagOpen name attrs):
                       (TS.TagText title): ts)
            | name == "a"   = do
                href <- eitherAttr attrs "href"
                Right (href, title, ts)
            | otherwise     = findHrefTitle ts
          where href = eitherAttr attrs "href"
        findHrefTitle (t:ts) = findHrefTitle ts
        findCount [] = Left "No count found"
        findCount ((TS.TagOpen name attrs):(TS.TagText txt):ts)
            | name == "div" && attrs == [("class", "subtle count")]
                        = readEither txt' >>= \i -> return (i, ts)
            | otherwise = findCount ts
          where txt' = takeWhile (/= ' ') . T.unpack $ txt
        findCount (t:ts) = findCount ts
getTitles (x:xs) = getTitles xs


-- -------------------------------- Utilities ------------------------------- --

eitherShow :: Show a => Either a b -> Either String b
eitherShow (Left x) = Left $ show x
eitherShow (Right x) = Right x


eitherAttr :: [(T.Text, T.Text)] -> T.Text
                  -> Either String T.Text
eitherAttr [] s = Left $ "could not find attr " ++ (T.unpack s)
eitherAttr ((k, v):xs) s
    | k == s    = Right v
    | otherwise = eitherAttr xs s


liftEither :: Either a b -> ExceptT a IO b
liftEither = ExceptT . return


findHeader :: HTH.HasHeaders a => a -> HTH.HeaderName -> Either String String
findHeader a h = fromMaybe (HTH.findHeader h a) ("Could not find header " ++ (show h))


fromMaybe :: Maybe b -> String -> Either String b
fromMaybe Nothing s = Left s
fromMaybe (Just x) _ = Right x


quote_plus :: String -> String
quote_plus = replacep20 . HTB.urlEncode


replacep20 :: String -> String
replacep20 [] = []
replacep20 (x:[]) = [x]
replacep20 (x:y:[]) = [x, y]
replacep20 (x:y:z:zs)
  | [x, y, z] == "%20" = '+' : (replacep20 zs)
  | otherwise          = x : (replacep20 (y:z:zs))
