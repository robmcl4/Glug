{-# LANGUAGE OverloadedStrings #-}

module API.Main (
  main
) where

import Network.HTTP.Types (status200, status404)
import Network.Wai
import Network.Wai.Handler.Warp
import Text.Read (readEither)


main :: IO ()
main = runEnv 4000 app


app :: Application
app req respond = case pathInfo req of
                    ["titles"] -> serveTitles req >>= respond
                    _ -> respond show404


serveTitles :: Request -> IO Response
serveTitles req = return $ responseLBS status200 [] "Hello, world"


show404 :: Response
show404 = responseLBS status404 [] ""
