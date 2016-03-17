{-# LANGUAGE OverloadedStrings #-}

module Downloader (
  candidateTitles
, Serializable (..)
, TitleCtx ()
, getSoup
, getTitles
)
where


import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B (w2c, c2w)
import qualified Data.ByteString.Lazy as BSL
import qualified Network.URI as URI
import qualified Network.HTTP as HTTP
import qualified Network.HTTP.Base as HTB
import qualified Network.HTTP.Headers as HTH
import qualified Text.HTML.TagSoup as TS
import Control.Monad (liftM)
import Data.List (find, group, sort)
import Text.Read (readEither)
import Network.BufferType (BufferType)
import Network.Stream (Result)


import Debug.Trace (traceShow)


searchurl = "http://subscene.com/subtitles/title?q="
subscenebase = "http://subscene.com"
useragent = "haskell/glug"


class  Serializable a  where
    serialize :: a -> B.ByteString
    deserialize :: B.ByteString -> Maybe a


data TitleCtx = TitleCtx { url :: String } deriving (Show, Eq)


instance Serializable TitleCtx where
    serialize = undefined
    deserialize = undefined


candidateTitles :: String -> IO (Either String [(String, Integer, TitleCtx)])
candidateTitles s = do
    soup <- getSoup $ searchurl ++ (quote_plus s)
    return $ soup >>= getTitles >>= Right . conv . dedup
  where dedup = map (head) . group . sort
        conv [] = []
        conv ((ttl, hrf, i):xs) = (bsl2str ttl, i, TitleCtx {url = bsl2str hrf}) : conv xs


getTitles :: [TS.Tag BSL.ByteString] -> Either String [(BSL.ByteString, BSL.ByteString, Integer)]
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
            | name == "a"   = href >>= \h -> Right (h, title, ts)
          where href = case find ((== "href") . fst) attrs of
                        Nothing -> Left "no href found"
                        Just (_, href) -> Right href
        findHrefTitle (t:ts) = findHrefTitle ts
        findCount [] = Left "No count found"
        findCount ((TS.TagOpen name attrs):(TS.TagText txt):ts)
            | name == "div" && attrs == [("class", "subtle count")] = do
                i <- readEither txt' :: Either String Integer
                Right (i, ts)
            | otherwise = findCount ts
          where txt' = takeWhile (/= ' ') . bsl2str $ txt
        findCount (t:ts) = findCount ts
getTitles (x:xs) = getTitles xs


getSoup :: String -> IO (Either String [TS.Tag BSL.ByteString])
getSoup s = makeGet s >>= \eith -> case eith of
                Left s -> return $ Left s
                Right bs -> return . Right $ TS.parseTags bs


makeGet :: String -> IO (Either String BSL.ByteString)
makeGet url = case uri of
                  Nothing -> return $ Left "Could not parse URI"
                  Just uri' -> makeGetReq (req uri')
    where uri = URI.parseURI url
          req u = HTH.replaceHeader HTH.HdrUserAgent useragent $ HTB.mkRequest HTB.GET u


makeGetReq :: HTB.Request BSL.ByteString -> IO (Either String BSL.ByteString)
makeGetReq r = HTTP.simpleHTTP r >>= \r' -> case r' of
                 Left c -> return . Left $ show c
                 Right resp -> case HTB.rspCode resp of
                                (2, _, _) -> return . Right $ HTB.rspBody resp
                                (3, _, _) -> case HTH.findHeader HTH.HdrLocation resp of
                                              Nothing -> return $ Left "No redirect location given"
                                              Just s -> makeGet s
                                _         -> return $ Left "bad status code"


bsl2str :: BSL.ByteString -> String
bsl2str = map (B.w2c) . BSL.unpack


quote_plus :: String -> String
quote_plus = replacep20 . HTB.urlEncode


replacep20 :: String -> String
replacep20 [] = []
replacep20 (x:[]) = [x]
replacep20 (x:y:[]) = [x, y]
replacep20 (x:y:z:zs)
  | [x, y, z] == "%20" = '+' : (replacep20 zs)
  | otherwise          = x : (replacep20 (y:z:zs))
