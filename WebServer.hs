{-# LANGUAGE OverloadedStrings #-}

module WebServer
( runWebServer
, webServerApp 
) where

import Network.Wai (Application, Response, Request, responseLBS, rawPathInfo, strictRequestBody)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)
import Network.HTTP.Base  (urlDecode)
import Control.Monad.State (liftIO)
import Network.XmlRpc.Server (XmlRpcMethod, handleCall, methods, fun)

import qualified Data.ByteString.Lazy as BsLazy
import qualified Data.ByteString.UTF8 as BsUTF
import qualified Data.Text as T
import qualified Data.Text.Encoding as E


type Port = Int

runWebServer :: Port -> IO ()
runWebServer port = do
  putStrLn $ "Server running at http://localhost:"++(show port)++"/"
  run port webServerApp

webServerApp :: Application
webServerApp request respond = respond $ echo (rawPathInfo request)


echo :: BsUTF.ByteString -> Response
echo path = jsonResponse $ "{\"cmd\":\""++(processPath path)++"\"}"


jsonResponse :: String -> Response
jsonResponse jsonStr = responseLBS status200 jsonContentType $ str2lazy jsonStr
  where jsonContentType = [("Content-Type", "application/json; charset=utf-8")]


processPath :: BsUTF.ByteString -> String
processPath = urlDecode . T.unpack . trimSlashes .  E.decodeUtf8


str2lazy :: String -> BsLazy.ByteString
str2lazy = BsLazy.fromStrict . BsUTF.fromString


trimSlashes :: T.Text -> T.Text
trimSlashes = T.dropWhile isSlash . T.dropWhileEnd isSlash
  where isSlash = (=='/')

--urlDecodeText :: T.Text -> T.Text
-- =



-- examples asi nanic pak

webServerApp_old :: Application
webServerApp_old request respond = respond $ case rawPathInfo request of
  "/"       -> index
  "/hello/" -> helloWorldJson 
  _         -> unknownCmd request


index :: Response
index = jsonResponse "{\"index\": true}"

helloWorldJson :: Response
helloWorldJson = jsonResponse "{\"hello\": \"world\"}"

unknownCmd :: Request -> Response
unknownCmd request = jsonResponse ("{\"error\": true, \"msg\": \"unknownCmd\", \"path\":"++ (show $ rawPathInfo request) ++"}")

