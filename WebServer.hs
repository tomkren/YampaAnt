{-# LANGUAGE OverloadedStrings #-}

module WebServer (runWebServer) where

import Network.Wai (Application, Response, responseLBS, rawPathInfo)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)
import Network.HTTP.Base  (urlDecode)

import qualified Data.ByteString.Lazy as BsLazy
import qualified Data.ByteString.UTF8 as BsUTF
import qualified Data.Text as T
import qualified Data.Text.Encoding as E

type Port = Int

--  example  -----------------------------------------------------------

exampleServerFun :: String -> String
exampleServerFun cmd = "{\"cmd\":\""++ cmd ++"\"}"

runExample :: IO ()
runExample = runWebServer 8080 exampleServerFun

--  exported functions  ------------------------------------------------

runWebServer :: Port -> (String -> String) -> IO ()
runWebServer port serverFun = do
  putStrLn $ "Server running at http://localhost:" ++ show port ++ "/"
  run port $ mkApp serverFun

------------------------------------------------------------------------

mkApp :: (String -> String) -> Application
mkApp serverFun request respond = respond . jsonResponse . serverFun . processPath . rawPathInfo $ request

jsonResponse :: String -> Response
jsonResponse jsonStr = responseLBS status200 jsonContentType $ str2lazy jsonStr
  where jsonContentType = [("Content-Type", "application/json; charset=utf-8")]

processPath :: BsUTF.ByteString -> String
processPath = urlDecode . T.unpack . trimSlashes . E.decodeUtf8

str2lazy :: String -> BsLazy.ByteString
str2lazy = BsLazy.fromStrict . BsUTF.fromString

trimSlashes :: T.Text -> T.Text
trimSlashes = T.dropWhile isSlash . T.dropWhileEnd isSlash
  where isSlash = (=='/')
