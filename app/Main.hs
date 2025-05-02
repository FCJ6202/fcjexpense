{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Servant
import Network.Wai.Handler.Warp (run)
import Domain.Action.User

-- Define the API type: GET /hello returns a String
type API = "hello" :> Get '[PlainText] String :<|> UserAPI


-- Implement the server handler
server :: Server API
server = return "Hello, Servant!" :<|> handler

-- Create the application
api :: Proxy API
api = Proxy

app :: Application
app = serve api server

main :: IO ()
main = do
  putStrLn "Running on http://localhost:3000"
  run 3000 app
