{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Main where

import Control.Concurrent
import Control.Monad (when)
import Data.List (nub)
import System.Console.CmdArgs.Implicit
import System.IO
import System.Directory
import System.Exit
import System.Posix.Unistd (sleep)
import System.Process

data Opts = Opts
    { command :: [String]
    , file :: [FilePath]
    , list :: FilePath
    } deriving (Data, Typeable, Show, Eq)

progOpts :: Opts
progOpts = Opts
    { command = def &= help "command(s) to execute; up to 10 (hotkeyed to 1-0)"
    , file = def &= help "file(s) to watch; can be repeated multiple times to define multiple files"
    , list = def &= help "list of files to watch"
    }
    &= details
        [ "Notes:"
        , ""
        ]

getOpts :: IO Opts
getOpts = cmdArgs $ progOpts
    &= summary (_PROGRAM_INFO ++ ", " ++ _COPYRIGHT)
    &= program _PROGRAM_NAME
    &= help _PROGRAM_DESC
    &= helpArg [explicit, name "help", name "h"]
    &= versionArg [explicit, name "version", name "v", summary _PROGRAM_INFO]

_PROGRAM_NAME, _PROGRAM_VERSION, _PROGRAM_INFO, _PROGRAM_DESC, _COPYRIGHT :: String
_PROGRAM_NAME = "auca"
_PROGRAM_VERSION = "0.0.1"
_PROGRAM_INFO = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION
_PROGRAM_DESC = "execute arbitrary command(s) based on file changes"
_COPYRIGHT = "(C) Linus Arver 2011"

data Color
    = Red
    | Green
    | Yellow
    | Cyan
    | Blue
    | CNone
    deriving (Show, Eq)

colorize :: Color -> String -> String
colorize c s = case c of
    Blue -> "\x1b[1;34m" ++ s ++ "\x1b[0m"
    Green -> "\x1b[1;32m" ++ s ++ "\x1b[0m"
    Red -> "\x1b[1;31m" ++ s ++ "\x1b[0m"
    Yellow -> "\x1b[1;33m" ++ s ++ "\x1b[0m"
    Cyan -> "\x1b[1;36m" ++ s ++ "\x1b[0m"
    _ -> s

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False -- disable terminal echo
    opts@Opts{..} <- getOpts
    errNo <- argsCheck opts
    when (errNo > 0) $ exitWith $ ExitFailure errNo
    files <- if null list
        then return []
        else do
            listContents <- readFile list
            return . filter (not . null) . lines $ listContents
    fs <- mapM doesFileExist file -- e.g., --file x --file y --file z
    flist <- mapM doesFileExist files -- e.g., --list x (and files defined in file x)
    errNo' <- filesCheck fs flist
    when (errNo' > 0) $ exitWith $ ExitFailure errNo
    helpMsg opts
    prog opts (nub $ file ++ files) -- combine files and remove duplicates for simplicity

argsCheck :: Opts -> IO Int
argsCheck Opts{..}
    | null command = errMsgNum "--command must be defined" 1
    | null file && null list = errMsgNum "either --file or --list must be defined" 1
    | otherwise = return 0

errMsgNum :: String -> Int -> IO Int
errMsgNum str num = errMsg str >> return num

-- Verify that the --file and --list arguments actually make sense.
filesCheck :: [Bool] -> [Bool] -> IO Int
filesCheck fs flist
    | any (==False) fs = errMsgNum "an argument to --file does not exist" 1
    | any (==False) flist = errMsgNum "a file defined in --list does not exist" 1
    | otherwise = return 0

prog :: Opts -> [FilePath] -> IO ()
prog opts filesToWatch = do
    let comDef = head $ command opts
    filesTS <- mapM getTimestamp filesToWatch
    _ <- forkIO $ loop opts comDef filesToWatch filesTS -- loop to handle file changes
    keyHandler opts comDef -- loop to handle key presses

getTimestamp :: FilePath -> IO String
getTimestamp f = do
    (_, sout, _, p) <- createProcess . cmd $ "ls --full-time " ++ f
    _ <- waitForProcess p
    sout' <- case sout of
        Just h -> hGetContents h
        Nothing -> return []
    return sout'

helpMsg :: Opts -> IO ()
helpMsg Opts{..} = do
    mapM_ showCom $ zip (map show [(1::Int)..10]) command
    putStrLn "press any key to execute the default command"
    putStrLn "press `h' for help"
    putStrLn "press `q' to quit"
    where
        showCom :: (String, String) -> IO ()
        showCom (a, b) = putStrLn $ "key " ++ squote (colorize Yellow a) ++ " set to " ++ squote (colorize Blue b)

loop :: Opts -> String -> [FilePath] -> [String] -> IO ()
loop o@Opts{..} comDef files filesTS = do
    _ <- sleep 1
    filesTS' <- mapM getTimestamp files
    when (filesTS /= filesTS') $ runCom $ cmd comDef
    loop o comDef files filesTS'

keyHandler :: Opts -> String -> IO ()
keyHandler o@Opts{..} comDef = do
    keyChar <- getChar
    case keyChar of
        'h' -> helpMsg o >> keyHandler o comDef
        'q' -> putStrLn [] >> return ()
        key -> do
            if elem key comKeys
                then case lookup [key] comHash of
                    Just com -> do
                        putStrLn $ "executing command " ++ squote (colorize Blue com)
                        runCom $ cmd com
                    _ -> do
                        putStrLn $ "command slot for key " ++ squote (colorize Yellow [key]) ++ " is empty"
                else do
                    runCom $ cmd comDef
            keyHandler o comDef
    where
        comHash :: [(String, String)]
        comHash = zip (map show [(1::Int)..10]) command
        comKeys :: String
        comKeys = concatMap show [(0::Int)..9]

runCom :: CreateProcess -> IO ()
runCom com = do
    (_, sout, _, p) <- createProcess com
    exitStatus <- waitForProcess p
    sout' <- case sout of
        Just h -> hGetContents h
        Nothing -> return []
    putStrLn sout'
    if (exitStatus == ExitSuccess)
        then do
            putStrLn $ colorize Green "command executed successfully"
        else do
            putStrLn $ colorize Red "command failed"

cmd :: String -> CreateProcess
cmd com = CreateProcess
    { cmdspec = ShellCommand $
        com ++ "| sed \"s/^/  " ++ colorize Cyan ">" ++ " /\""
    , cwd = Nothing
    , env = Nothing
    , std_in = CreatePipe
    , std_out = CreatePipe
    , std_err = Inherit
    , close_fds = True
    }

errMsg :: String -> IO ()
errMsg msg = hPutStrLn stderr $ "error: " ++ msg

squote :: String -> String
squote s = "`" ++ s ++ "'"
