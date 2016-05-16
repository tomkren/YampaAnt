{-# LANGUAGE OverloadedStrings #-}

module XmlRpcServer
( runXmlRpcServer
, xmlRpcApp 
) where

import Network.Wai (Application, responseLBS, strictRequestBody)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)
import Control.Monad.State (liftIO)
import Network.XmlRpc.Server (XmlRpcMethod, handleCall, methods, fun)
import qualified Data.ByteString.Lazy.Char8 as BS






-----------------------------

type Port = Int

runXmlRpcServer :: Port -> [(String, XmlRpcMethod)] -> IO ()
runXmlRpcServer port meths = do
  putStrLn $ "xml-rpc server at http://localhost:" ++ (show port) ++"/"
  putStr   $ "avalible methods:\n" ++ (concatMap (\(m,_)->"  "++m++"\n") meths)
  run port $ xmlRpcApp meths

xmlRpcApp :: [(String, XmlRpcMethod)] -> Application
xmlRpcApp ms request respond = do
  reqBody <- strictRequestBody request
  let reqStr = BS.unpack reqBody
  output <- liftIO $ handleCall (methods ms) reqStr
  respond $ responseLBS status200 [] output




-- Example --------------------------------------------

runExample :: IO ()
runExample = runXmlRpcServer 8080 [("add", fun add),("times", fun times)]

add :: Int -> Int -> IO Int
add x y = return (x + y)

times :: Int -> Int -> IO Int
times x y = return (x * y)




{-

main2 :: IO ()
main2 = do
    putStrLn $ "http://localhost:8080/"
    run 8080 app3

app3 :: Application
app3 request respond = respond $ case rawPathInfo request of
    "/"    -> index
    "/raw" -> plainIndex
    _      -> notFound

index :: Response
index = responseFile
    status200
    [("Content-Type", "text/html")]
    "www/index.html"
    Nothing

plainIndex :: Response
plainIndex = responseFile
    status200
    [("Content-Type", "text/plain")]
    "www/index.html"
    Nothing

notFound :: Response
notFound = responseLBS
    status404
    [("Content-Type", "text/plain")]
    "404 - Not Found :("

app2 :: Application
app2 _ respond = respond index

app :: Application
app _ respond = do
    putStrLn "I've done some IO here"
    respond $ responseLBS
        status200
        [("Content-Type", "text/plain")]
        "Hello, Web!"

-}



