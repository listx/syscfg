{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Proxy
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Version (showVersion)
import Network.HTTP.Client (newManager, defaultManagerSettings, Manager)
import Options.Applicative
import Servant.API
import Servant.Client

import Paths_lhx (version)
import LHX.GitVersion

type Message = T.Text

type LHAPI = "ping" :> Get '[PlainText] Message -- GET /ping

lhApi :: Proxy LHAPI
lhApi = Proxy

getPing :: ClientM Message
getPing = client lhApi

newtype Opts = Opts
  { subcommand :: Subcommand }

data Subcommand
  = Ping
  | PathShorten PathShortenOpts

data PathShortenOpts = PathShortenOpts
  { pathAliases :: FilePath
  }

optionsP :: Parser Opts
optionsP = Opts <$> subcommandP

subcommandP :: Parser Subcommand
subcommandP = hsubparser
  ( command "ping" (info (pure Ping) (progDesc "Check lh server connectivity"))
  <> command "path-shorten" (info (PathShorten <$> pathShortenOptsP) (progDesc "Shorten a path"))
  <> metavar "SUBCOMMAND"
  )

pathShortenOptsP :: Parser PathShortenOpts
pathShortenOptsP
  = PathShortenOpts
  <$> pathAliasesP

pathAliasesP :: Parser FilePath
pathAliasesP = strOption
  ( long "path-aliases"
  <> short 'a'
  <> value ""
  <> metavar "FILE"
  <> help "file containing path aliases"
  )

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

optsHandler :: Opts -> Manager -> IO ()
optsHandler (Opts subcommand) manager' = case subcommand of
  PathShorten _ -> putStrLn "path-shorten wanted"
  Ping -> ping manager'

ping :: Manager -> IO ()
ping manager' = do
  res <- runClientM getPing (mkClientEnv manager' (BaseUrl Http "localhost" 8080 ""))
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right msg -> T.putStrLn msg
