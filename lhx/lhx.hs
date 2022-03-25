{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Proxy
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import Servant.Client

type Message = T.Text

type LHAPI = "ping" :> Get '[PlainText] Message -- GET /ping

lhApi :: Proxy LHAPI
lhApi = Proxy

getPing :: ClientM Message
getPing = client lhApi

main :: IO ()
main = do
  manager' <- newManager defaultManagerSettings
  res <- runClientM getPing (mkClientEnv manager' (BaseUrl Http "localhost" 8080 ""))
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right msg -> T.putStrLn msg
