{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.ByteString qualified as BS
import Data.Proxy
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import Servant.Client

type Message = BS.ByteString

type MyApi = "ping" :> Get '[OctetStream] Message -- GET /ping

myApi :: Proxy MyApi
myApi = Proxy

getPing :: ClientM Message
getPing = client myApi

main :: IO ()
main = do
  manager' <- newManager defaultManagerSettings
  res <- runClientM getPing (mkClientEnv manager' (BaseUrl Http "localhost" 8080 ""))
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right msg -> BS.putStr msg
