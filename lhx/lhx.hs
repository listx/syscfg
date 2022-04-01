{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Aeson
import Data.HashMap.Strict qualified as H
import Data.Proxy
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Version (showVersion)
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings, Manager)
import Options.Applicative
import Servant.API
import Servant.Client
import System.Environment

import Paths_lhx (version)
import LHX.GitVersion

type Message = T.Text

type LHAPI = "path-shorten" :> ReqBody '[JSON] PathShortenReqBody :> Post '[JSON] PathShortened -- POST /path-shorten
        :<|> "ping" :> GetNoContent -- GET /ping

lhApi :: Proxy LHAPI
lhApi = Proxy

data PathShortenReqBody = PathShortenReqBody
  { name :: FilePath -- the filepath to shorten
  , aliases_raw :: T.Text
  , substitutions :: H.HashMap T.Text T.Text -- e.g., ("$HOME", "/home/foo")
  } deriving (Generic, Show)

instance ToJSON PathShortenReqBody

data PathShortened = PathShortened
  { path_shortened :: FilePath
  } deriving (Generic, Show)

instance FromJSON PathShortened

-- These are the client "action" functions. All we need to do is provide a type
-- signature, and Servant generates the implementation. We can then pass in one
-- of these actions depending on what we want to do in "runClient".
postPathShorten :: PathShortenReqBody -> ClientM PathShortened
getPing :: ClientM NoContent
(postPathShorten :<|> getPing) = client lhApi

newtype Opts = Opts
  { subcommand :: Subcommand }

data Subcommand
  = PathShorten PathShortenOpts
  | Ping

data PathShortenOpts = PathShortenOpts
  { name :: FilePath
  , pathAliases :: FilePath
  }

optionsP :: Parser Opts
optionsP = Opts <$> subcommandP

subcommandP :: Parser Subcommand
subcommandP = hsubparser
  ( command "path-shorten" (info (PathShorten <$> pathShortenOptsP) (progDesc "Shorten a path"))
  <> command "ping" (info (pure Ping) (progDesc "Check lh server connectivity"))
  <> metavar "SUBCOMMAND"
  )

pathShortenOptsP :: Parser PathShortenOpts
pathShortenOptsP
  = PathShortenOpts
  <$> (argument str (metavar "FILEPATH"))
  <*> pathAliasesP

pathAliasesP :: Parser FilePath
pathAliasesP = strOption
  ( long "path-aliases"
  <> short 'a'
  <> metavar "FILE"
  <> help "file containing path aliases"
  )

runClient :: ClientEnv -> ClientM a -> (a -> IO ()) -> IO ()
runClient clientEnv action_ valHandler = do
  res <- runClientM action_ clientEnv
  case res of
    Left err -> T.putStrLn $ "Error: " <> (T.pack $ show err)
    Right val -> valHandler val

optsHandler :: Opts -> Manager -> IO ()
optsHandler (Opts subcommand) mgr = do
  let
    clientEnv = mkClientEnv mgr (BaseUrl Http "localhost" 8080 "")
    resolve = runClient clientEnv
  case subcommand of
    PathShorten pso -> do
      pa <- T.readFile pso.pathAliases
      home <- T.pack <$> getEnv "HOME"
      let
        pathShortenReqBody = PathShortenReqBody
          { name = pso.name
          , aliases_raw = pa
          , substitutions = H.fromList [("$HOME", home), (home, "~")]
          }
      resolve (postPathShorten pathShortenReqBody) (T.putStr . T.pack . (.path_shortened))
    Ping -> resolve getPing (\_ -> T.putStrLn "OK")

main :: IO ()
main = do
  opts <- customExecParser (prefs showHelpOnEmpty) optsP
  manager' <- newManager defaultManagerSettings
  optsHandler opts manager'
  where
  optsP = info parserProgramOptions infoMod
  parserProgramOptions = helper
    <*> versionOption
    <*> optionsP
  infoMod = fullDesc
    <> header "lhx - a client for lh"
  versionOption = infoOption
    (concat [showVersion version, "-g", $(gitVersion)])
    (long "version" <> short 'v' <> help "Show version")
