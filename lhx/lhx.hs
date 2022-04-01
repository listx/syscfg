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
import System.Exit
import System.IO

import Paths_lhx (version)
import LHX.GitVersion

type Message = T.Text

type LHAPI = "path-shorten" :> ReqBody '[JSON] PathShortenReqBody :> Post '[JSON] PathShortened -- POST /path-shorten
        :<|> "paths-sort" :> ReqBody '[JSON] PathsSortReqBody :> Post '[JSON] PathsSorted -- POST /paths-sort
        :<|> "ping" :> GetNoContent -- GET /ping
        :<|> "shutdown" :> GetNoContent -- GET /shutdown

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

data PathsSortReqBody = PathsSortReqBody
  { paths :: FilePath -- the $PATH to sort
  , priorities_raw :: T.Text
  , home :: T.Text -- $HOME
  } deriving (Generic, Show)

instance ToJSON PathsSortReqBody

data PathsSorted = PathsSorted
  { paths_sorted :: FilePath
  } deriving (Generic, Show)

instance FromJSON PathsSorted

-- These are the client "action" functions. All we need to do is provide a type
-- signature, and Servant generates the implementation. We can then pass in one
-- of these actions depending on what we want to do in "runClient".
postPathShorten :: PathShortenReqBody -> ClientM PathShortened
postPathsSort :: PathsSortReqBody -> ClientM PathsSorted
getPing :: ClientM NoContent
getShutdown :: ClientM NoContent
(postPathShorten :<|> postPathsSort :<|> getPing :<|> getShutdown) = client lhApi

newtype Opts = Opts
  { subcommand :: Subcommand }

data Subcommand
  = PathShorten PathShortenOpts
  | PathsSort PathsSortOpts
  | Ping
  | Shutdown

data PathShortenOpts = PathShortenOpts
  { name :: FilePath
  , pathAliases :: FilePath
  }

data PathsSortOpts = PathsSortOpts
  { paths :: FilePath
  , prioritiesRaw :: FilePath
  }

optionsP :: Parser Opts
optionsP = Opts <$> subcommandP

subcommandP :: Parser Subcommand
subcommandP = hsubparser
  ( command "path-shorten" (info (PathShorten <$> pathShortenOptsP) (progDesc "Shorten a path"))
  <> command "paths-sort" (info (PathsSort <$> pathsSortOptsP) (progDesc "Sort the $PATH"))
  <> command "ping" (info (pure Ping) (progDesc "Check lh server connectivity"))
  <> command "shutdown" (info (pure Shutdown) (progDesc "Shut down lh server instance"))
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

pathsSortOptsP :: Parser PathsSortOpts
pathsSortOptsP
  = PathsSortOpts
  <$> (argument str (metavar "$PATH"))
  <*> prioritiesRawP


prioritiesRawP :: Parser FilePath
prioritiesRawP = strOption
  ( long "priorities-raw"
  <> short 'p'
  -- NOTE: adding "value" (default value) makes this option not mandatory.
  <> value ""
  <> metavar "FILE"
  <> help "file containing path priorities (for sorting $PATH)"
  )

runClient :: ClientEnv -> ClientM a -> (a -> IO ()) -> IO ()
runClient clientEnv action_ valHandler = do
  res <- runClientM action_ clientEnv
  case res of
    Left err -> do
      T.hPutStrLn stderr $ "Error: " <> (T.pack $ show err)
      exitFailure
    Right val -> valHandler val

optsHandler :: Opts -> Manager -> IO ()
optsHandler (Opts subcommand) mgr = do
  let
    clientEnv = mkClientEnv mgr (BaseUrl Http "localhost" 8080 "")
    resolve = runClient clientEnv
  home <- T.pack <$> getEnv "HOME"
  case subcommand of
    PathShorten o -> do
      pa <- T.readFile o.pathAliases
      let
        pathShortenReqBody = PathShortenReqBody
          { name = o.name
          , aliases_raw = pa
          , substitutions = H.fromList [("$HOME", home), (home, "~")]
          }
      resolve (postPathShorten pathShortenReqBody) (T.putStr . T.pack . (.path_shortened))
    PathsSort o -> do
      prio <- if null o.prioritiesRaw
        then return ""
        else T.readFile o.prioritiesRaw
      let
        pathsSortReqBody = PathsSortReqBody
          { paths = o.paths
          , priorities_raw = prio
          , home = home
          }
      resolve (postPathsSort pathsSortReqBody) (T.putStr . T.pack . (.paths_sorted))
    Ping -> resolve getPing (\_ -> T.putStrLn "OK")
    Shutdown -> resolve getShutdown (\_ -> T.putStrLn "Shutting down...")

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
