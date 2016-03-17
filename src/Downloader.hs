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
import Data.List (find, group, sort)
import Text.Read (readEither)
import Network.BufferType (BufferType)
import Network.Stream (Result)
import Control.Monad.Except


import Debug.Trace (traceShow)


searchurl = "http://subscene.com/subtitles/title?q="
subscenebase = "http://subscene.com"
useragent = "haskell/glug"


class  Serializable a  where
    serialize :: a -> B.ByteString
    deserialize :: B.ByteString -> Maybe a


data TitleCtx = TitleCtx { url :: String } deriving (Show, Eq)


instance Serializable TitleCtx where
    serialize (TitleCtx url) =  B.pack . map (B.c2w) $ url
    deserialize = Just . TitleCtx . (map (B.w2c)) . B.unpack


candidateTitles :: String -> IO (Either String [(String, Integer, TitleCtx)])
candidateTitles s = runExceptT $ do
    soup <- getSoup $ searchurl ++ (quote_plus s)
    titles <- liftEither $ getTitles soup
    return . conv . dedup $ titles
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


getSoup :: String -> ExceptT String IO ([TS.Tag BSL.ByteString])
getSoup s = makeGet s >>= return . TS.parseTags


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

----------------- Utilities -----------------

liftEither :: Either a b -> ExceptT a IO b
liftEither = ExceptT . return


findHeader :: HTH.HasHeaders a => a -> HTH.HeaderName -> Either String String
findHeader a h = fromMaybe (HTH.findHeader h a) ("Could not find header " ++ (show h))


fromMaybe :: Maybe b -> String -> Either String b
fromMaybe Nothing s = Left s
fromMaybe (Just x) _ = Right x


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
