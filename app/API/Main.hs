{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module API.Main (
  main
) where

import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import qualified Glug as G

import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Data.Aeson (encode, ToJSON)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Time.Clock
import Data.Time.Format (formatTime, defaultTimeLocale)
import GHC.Generics
import Network.HTTP.Types (status200, status404, status500, methodGet, Status)
import Network.HTTP.Types.Header (Header, hContentType, hUserAgent)
import Network.Wai
import Network.Wai.Handler.Warp
import System.Environment (lookupEnv)
import Text.Read (readEither, readMaybe)


import API.Helpers
import API.CacheMaintenance
import Glug (Cache, newCache)


main :: IO ()
main = do
    let cache = newCache cacheSize
    mvar <- newMVar cache
    chan <- newChan
    maintainCache chan mvar
    settings <- getSettings
    runSettings settings $ app mvar chan


app :: MVar Cache -> Chan Cache -> Application
app mvar chan req respond = do
                cache <- readMVar mvar
                case pathInfo req of
                    ["titles",_] -> serveTitles cache chan req >>= respond
                    ["words",_]  -> serveSubs cache chan req >>= respond
                    ["title",_] -> serveTitleDetails cache chan req >>= respond
                    _ -> respond show404


getSettings :: IO Settings
getSettings = do
          p <- fromIntegral <$> port
          return $
            setBeforeMainLoop beforeMainLoop $
            setLogger logReq $
            setPort p defaultSettings


port :: IO Integer
port = do
          portMayStr <- lookupEnv "PORT"
          return . fromMaybe 3000 $ portMayStr >>= readMaybe


-- | The max number of items in the cache
cacheSize :: Int
cacheSize = 256


beforeMainLoop :: IO ()
beforeMainLoop = do
          portInt <- port
          putStrLn $ "Waiting for connections on port " ++ show portInt


logReq :: Request -> Status -> Maybe Integer -> IO ()
logReq req _ _ = t >>= (\t' -> putStrLn $ t' ++ " :: " ++ method ++ " " ++ path ++ qs ++ " :: " ++ ua)
    where method = T.unpack . Enc.decodeUtf8 $ requestMethod req
          path = T.unpack . Enc.decodeUtf8 $ rawPathInfo req
          qs = T.unpack . Enc.decodeUtf8 $ rawQueryString req
          ua = case find (\(k, _) -> k == hUserAgent) $ requestHeaders req of
                 Just (_, x) -> T.unpack . Enc.decodeUtf8 $ x
                 Nothing     -> ""
          t = formatTime defaultTimeLocale "%FT%X" <$> getCurrentTime


-- ---------------------------- Request Handlers ---------------------------- --

serveTitles :: Cache -> Chan Cache -> Request -> IO Response
serveTitles cache chan req = if requestMethod req /= methodGet
    then return show404
    else case pathInfo req of
             [_, title] -> do
                 (ttls, cache') <- G.execMonadGlugIO cache $ getTitles . T.unpack $ title
                 writeChan chan cache'
                 case ttls of
                     Left s -> return $ responseLBS status500 hdrJson (errMsg . T.pack $ s)
                     Right x -> return . responseLBS status200 hdrJson . encode $ x
             _            -> return show404


serveSubs :: Cache -> Chan Cache -> Request -> IO Response
serveSubs cache chan req = if requestMethod req /= methodGet
    then return show404
    else case pathInfo req of
            [_, ref] -> do
              (best, cache') <- G.execMonadGlugIO cache $ getBestWords (T.unpack ref) rng
              writeChan chan cache'
              case best of
                Left s  -> return $ responseLBS status500 hdrJson (errMsg . T.pack $ s)
                Right x -> return . responseLBS status200 hdrJson . encode $ x
            _          -> return show404
  where hi = min 13 . fromMaybe 6 $ queryParam req "min" >>= maybeI
        lo = max 1 . fromMaybe 3 $ queryParam req "max" >>= maybeI
        rng = (lo, hi)
        maybeI b = eToM (Enc.decodeUtf8' (B.toStrict b)) >>= eToM . readEither . T.unpack


serveTitleDetails :: Cache -> Chan Cache -> Request -> IO Response
serveTitleDetails cache chan req = if requestMethod req /= methodGet
    then return show404
    else case pathInfo req of
          [_, url] -> if not . isImdbId $ url
                then return show404
                else do
                  (id_, cache') <- G.execMonadGlugIO cache $ getTitleDetails . T.unpack $ url
                  writeChan chan cache'
                  case id_ of
                    Left s  -> return $ responseLBS status500 hdrJson (errMsg . T.pack $ s)
                    Right x -> return . responseLBS status200 hdrJson . encode $ x
          _        -> return show404


show404 :: Response
show404 = responseLBS status404 hdrJson (errMsg "404 not found")


-- ------------------------------- Utilities ------------------------------- --

data ErrorMessage = ErrorMessage { message :: T.Text }
                                 deriving (Show, Eq, Generic)
instance ToJSON ErrorMessage


eToM :: Either a b -> Maybe b
eToM (Left _)  = Nothing
eToM (Right a) = Just a


errMsg :: T.Text -> B.ByteString
errMsg t = encode ErrorMessage { message = t }


hdrJson :: [Header]
hdrJson = [(hContentType, "application/json")
          ,("Access-Control-Allow-Origin", "*")]


-- | gets the given query parameter from the request's query string
queryParam :: Request -> B.ByteString -> Maybe B.ByteString
queryParam r s = do
      found <- find f . queryString $ r
      bs <- snd found
      return . B.fromStrict $ bs
    where f = (==) s' . fst
          s' = B.toStrict s
