{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Servant
import Network.Wai.Handler.Warp (run)

-- Define the API type: GET /hello returns a String
type API = "hello" :> Get '[PlainText] String

-- Implement the server handler
server :: Server API
server = return "Hello, Servant!"

-- Create the application
api :: Proxy API
api = Proxy

app :: Application
app = serve api server

main :: IO ()
main = do
  putStrLn "Running on http://localhost:8080/hello"
  run 8080 app
