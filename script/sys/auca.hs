{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Main where

import Control.Concurrent
import Control.Monad (when)
import Data.List (nub)
import Data.Time.LocalTime
import System.Console.CmdArgs.Implicit
import System.IO
import System.Directory
import System.Environment
import System.Exit
import System.Posix.Unistd (sleep)
import System.Process

data Opts = Opts
    { command :: [String]
    , command_simple :: String
    , file :: [FilePath]
    , list :: FilePath
    , interval :: Int
    } deriving (Data, Typeable, Show, Eq)

progOpts :: Opts
progOpts = Opts
    { command = def &= typ "COMMAND" &= help "command(s) to execute; up to 10 (hotkeyed to 1-0)"
    , command_simple = def &= typ "COMMAND" &= name "C" &= help "command to execute; it takes the first file, and calls command after it; e.g., `-C lilypond -f foo.ly' will translate to `lilypond foo.ly' as the default command"
    , file = def &= help "file(s) to watch; can be repeated multiple times to define multiple files"
    , list = def &= help "list of files to watch"
    , interval = 0 &= typ "SECONDS" &= help "sleep SECONDS amount of time and detect changes only on these intervals"
    }
    &= details
        [ "Notes:"
        , ""
        , "  All commands are passed to the default shell."
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
_PROGRAM_VERSION = "0.0.2"
_PROGRAM_INFO = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION
_PROGRAM_DESC = "execute arbitrary command(s) based on file changes"
_COPYRIGHT = "(C) Linus Arver 2011"

data Color
    = Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    deriving (Show, Eq)

colorize :: Color -> String -> String
colorize c s = c' ++ s ++ e
    where
    c' = "\x1b[" ++ case c of
        Red -> "1;31m"
        Green -> "1;32m"
        Yellow -> "1;33m"
        Blue -> "1;34m"
        Magenta -> "1;35m"
        Cyan -> "1;36m"
    e = "\x1b[0m"

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False -- disable terminal echo
    args' <- getArgs
    opts@Opts{..} <- (if null args' then withArgs ["--help"] else id) $ getOpts
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
    let filesMaster = nub $ file ++ files
    helpMsg opts (head filesMaster)
    prog opts filesMaster

argsCheck :: Opts -> IO Int
argsCheck Opts{..}
    | null command && null command_simple = errMsgNum "--command or --command-simple must be defined" 1
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
prog opts@Opts{..} filesToWatch = do
    let comDef = if null command_simple
        then (head command)
        else command_simple ++ " " ++ (head filesToWatch)
    filesTS <- mapM getTimestamp filesToWatch
    _ <- forkIO $ loop opts comDef filesToWatch filesTS -- loop to handle file changes
    keyHandler opts comDef (head filesToWatch) -- loop to handle key presses

getTimestamp :: FilePath -> IO String
getTimestamp f = do
    (_, sout, _, p) <- createProcess . cmdQuiet $ "ls --full-time " ++ f
    _ <- waitForProcess p
    sout' <- case sout of
        Just h -> hGetContents h
        Nothing -> return []
    return sout'

helpMsg :: Opts -> FilePath -> IO ()
helpMsg Opts{..} f = do
    mapM_ showCom $ if null command
        then [("1", command_simple ++ " " ++ f)]
        else zip (map show [(1::Int)..10]) command
    putStrLn "press `h' for help"
    putStrLn "press `q' to quit"
    putStrLn $ "press any other key to execute the default command " ++
        squote (colorize Blue comDef)
    where
        showCom :: (String, String) -> IO ()
        showCom (a, b) = putStrLn $ "key " ++ squote (colorize Yellow a) ++ " set to " ++ squote (colorize Blue b)
        comDef = if null command
            then command_simple ++ " " ++ f
            else head command

loop :: Opts -> String -> [FilePath] -> [String] -> IO ()
loop o@Opts{..} comDef files filesTS = do
    _ <- sleep (if interval > 0 then interval else 1)
    filesTS' <- mapM getTimestamp files
    when (filesTS /= filesTS') $ do
        putStrLn []
        showTime
        putStr $ colorize Magenta ": change detected"
        putStrLn $ "; executing command " ++ squote (colorize Blue comDef)
        runCom $ cmd comDef
    loop o comDef files filesTS'

keyHandler :: Opts -> String -> FilePath -> IO ()
keyHandler o@Opts{..} comDef f = do
    keyChar <- getChar
    case keyChar of
        'h' -> helpMsg o f >> keyHandler o comDef f
        'q' -> putStrLn [] >> return ()
        key -> do
            if elem key comKeys
                then case lookup [key] comHash of
                    Just com -> do
                        putStrLn []
                        showTime
                        putStr $ ": " ++ colorize Cyan "manual override" ++ " (slot " ++ colorize Yellow [key] ++ ")"
                        putStrLn $ "; executing command " ++ squote (colorize Blue com)
                        runCom $ cmd com
                    _ -> do
                        putStrLn []
                        putStrLn $ "command slot for key " ++ squote (colorize Yellow [key]) ++ " is empty"
                else do
                    putStrLn []
                    showTime
                    putStr $ ": " ++ colorize Cyan "manual override"
                    putStrLn $ "; executing command " ++ squote (colorize Blue comDef)
                    runCom $ cmd comDef
            keyHandler o comDef f
    where
        comHash :: [(String, String)]
        comHash = if null command
            then [("1", command_simple ++ " " ++ f)]
            else zip (map show [(1::Int)..10]) command
        comKeys :: String
        comKeys = concatMap show [(0::Int)..9]

runCom :: CreateProcess -> IO ()
runCom com = do
    (_, _, _, p) <- createProcess com
    exitStatus <- waitForProcess p
    showTime
    putStrLn $ ": " ++ if (exitStatus == ExitSuccess)
        then colorize Green "command executed successfully"
        else colorize Red "command failed"

cmd :: String -> CreateProcess
cmd com = CreateProcess
    { cmdspec = ShellCommand $
        (com ++ " 2>&1 | sed \"s/^/  " ++ colorize Cyan ">" ++ " /\"")
    , cwd = Nothing
    , env = Nothing
    , std_in = CreatePipe
    , std_out = Inherit
    , std_err = Inherit
    , close_fds = True
    }

cmdQuiet :: String -> CreateProcess
cmdQuiet com = CreateProcess
    { cmdspec = ShellCommand com
    , cwd = Nothing
    , env = Nothing
    , std_in = CreatePipe
    , std_out = CreatePipe
    , std_err = Inherit
    , close_fds = True
    }

showTime :: IO ()
showTime = getZonedTime >>= putStr . show

errMsg :: String -> IO ()
errMsg msg = hPutStrLn stderr $ "error: " ++ msg

squote :: String -> String
squote s = "`" ++ s ++ "'"
