{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module API.Main (
  main
) where

import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc

import Control.Monad (liftM)
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


main :: IO ()
main = runSettings settings app


app :: Application
app req respond = case pathInfo req of
                    ("titles":_:[]) -> serveTitles req >>= respond
                    ("words":_:[])  -> serveSubs req >>= respond
                    ("title":_:[]) -> serveTitleDetails req >>= respond
                    _ -> respond show404


settings :: Settings
settings = setBeforeMainLoop (beforeMainLoop) $ setLogger logReq defaultSettings


port :: IO (Integer)
port = do
          portMayStr <- lookupEnv "PORT" 
          return $ case portMayStr >>= readMaybe of
              Just x -> x 
              Nothing -> 3000


beforeMainLoop :: IO ()
beforeMainLoop = do
          portInt <- port
          putStrLn $ "Waiting for connections on port " ++ (show portInt)


logReq :: Request -> Status -> Maybe Integer -> IO ()
logReq req _ _ = t >>= (\t' -> putStrLn $ t' ++ " :: " ++ method ++ " " ++ path ++ qs ++ " :: " ++ ua)
    where method = T.unpack . Enc.decodeUtf8 $ requestMethod req
          path = T.unpack . Enc.decodeUtf8 $ rawPathInfo req
          qs = T.unpack . Enc.decodeUtf8 $ rawQueryString req
          ua = case find (\(k, _) -> k == hUserAgent) $ requestHeaders req of
                 Just (_, x) -> T.unpack . Enc.decodeUtf8 $ x
                 Nothing     -> ""
          t = getCurrentTime >>= return . formatTime defaultTimeLocale "%FT%X"


-- ---------------------------- Request Handlers ---------------------------- --

serveTitles :: Request -> IO Response
serveTitles req = do
    if requestMethod req /= methodGet
    then return show404
    else case pathInfo req of
             (_:title:[]) -> do
                 ttls <- getTitles . T.unpack $ title
                 case ttls of
                     Left s -> return $ responseLBS status500 hdrJson (errMsg . T.pack $ s)
                     Right x -> return . responseLBS status200 hdrJson . encode $ x
             _            -> return show404


serveSubs :: Request -> IO Response
serveSubs req = do
    if requestMethod req /= methodGet
    then return show404
    else case pathInfo req of
            (_:url:[]) -> do
                if not . isSubLink $ url
                then return show404
                else do
                  best <- getBestWords (T.unpack url) rng
                  case best of
                    Left s  -> return $ responseLBS status500 hdrJson (errMsg . T.pack $ s)
                    Right x -> return . responseLBS status200 hdrJson . encode $ x
            _          -> return show404
  where hi = fromMaybe 6 $ queryParam req "min" >>= maybeI >>= liftM (min 13)
        lo = fromMaybe 3 $ queryParam req "max" >>= maybeI >>= liftM (max 1)
        rng = (lo, hi)
        maybeI b = eToM (Enc.decodeUtf8' (B.toStrict b)) >>= eToM . readEither . T.unpack


serveTitleDetails :: Request -> IO Response
serveTitleDetails req = do
    if requestMethod req /= methodGet
    then return show404
    else case pathInfo req of
          (_:url:[]) -> do
              if not . isImdbId $ url
                then return show404
                else do
                  id_ <- getTitleDetails . T.unpack $ url
                  case id_ of
                    Left s  -> return $ responseLBS status500 hdrJson (errMsg . T.pack $ s)
                    Right x -> return . responseLBS status200 hdrJson . encode $ x
          _          -> return show404


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
          ,(("Access-Control-Allow-Origin", "*"))]


-- | gets the given query parameter from the request's query string
queryParam :: Request -> B.ByteString -> Maybe B.ByteString
queryParam r s = do
      found <- find f . queryString $ r
      bs <- snd found
      return . B.fromStrict $ bs
    where f = (==) s' . fst
          s' = B.toStrict s
