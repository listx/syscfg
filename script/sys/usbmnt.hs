{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Main where

import System.Console.CmdArgs.Implicit
import System.IO
import System.Environment
import System.Exit
import System.Process
import Control.Monad (when)
import Char

data Opts = Opts
    { device :: String
    , fsys :: String
    } deriving (Data, Typeable, Show, Eq)

progOpts :: Opts
progOpts = Opts
    { device = "" &= typ "DEVICE" &= help "the device letter in the /dev/sdX naming scheme, where X is the device letter (if more than 26 devices, then aa, ab, etc.); default \"b\""
    , fsys = "vfat" &= typ "FILE SYSTEM" &= help "file system of the device; valid inputs are ext2 or vfat; default is vfat"
    }

getOpts :: IO Opts
getOpts = cmdArgs $ progOpts
    &= summary (_PROGRAM_INFO ++ ", " ++ _COPYRIGHT)
    &= program _PROGRAM_NAME
    &= help _PROGRAM_DESC
    &= helpArg [explicit, name "help", name "h"]
    &= versionArg [explicit, name "version", name "v", summary _PROGRAM_INFO]

_PROGRAM_NAME, _PROGRAM_VERSION, _PROGRAM_INFO, _PROGRAM_DESC, _COPYRIGHT :: String
_PROGRAM_NAME = "usbmnt"
_PROGRAM_VERSION = "0.0.1"
_PROGRAM_INFO = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION
_PROGRAM_DESC = "mounts a USB flash drive"
_COPYRIGHT = "(C) Linus Arver 2011"

_FILE_SYSTEMS :: [String]
_FILE_SYSTEMS = [ "ext2"
                , "vfat"
                ]

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering
    arguments <- getArgs
    opts <- (if null arguments then withArgs ["--help"] else id) getOpts
    user <- getEnv "USER"
    errNo <- argsCheck opts user
    when (errNo > 0) $ exitWith $ ExitFailure errNo
    prog opts user

argsCheck :: Opts -> String -> IO Int
argsCheck Opts{..} user
    | null user = e "could not get environment variable $USER" 1
    | not (null device) && not (all isAlphaLower device) = e "device must be one or more lowercase ASCII characters" 1
    | not $ elem fsys _FILE_SYSTEMS = e ("file system must be one of " ++ unwords _FILE_SYSTEMS) 1
    | otherwise = return 0
    where
        e :: String -> Int -> IO Int
        e str num = errMsg str >> return num
        isAlphaLower :: Char -> Bool
        isAlphaLower c = 'a' >= c && c <= 'z'

prog :: Opts -> String -> IO ()
prog Opts{..} user
    -- specific device requested
    | not (null device) = do
        (_, _, _, p) <- createProcess $ cmd argExtra (devicePath device)
        _ <- waitForProcess p
        return ()
    -- look for devices sdb, sdc, sdd, etc.
    | otherwise = do
        devicesAvail <- getDevices
        mapM_ tryMount devicesAvail
        errMsg $ "discovered devices are already mounted"
        exitWith (ExitFailure 1)
    where
        argExtra = if fsys == "ext2"
            then "ext2 -o rw,relatime"
            else ("vfat -o rw,uid=" ++ user ++ ",gid=" ++ user)
        devicePath :: String -> String
        devicePath dev = "/dev/sd" ++ dev ++ "1"
        cmd arg devP = CreateProcess
            { cmdspec = ShellCommand ("sudo mount -t " ++ arg ++ " " ++ devP ++ " /mnt/u0 &>/dev/null")
            , cwd = Nothing
            , env = Nothing
            , std_in = CreatePipe
            , std_out = CreatePipe
            , std_err = Inherit
            , close_fds = False
            }
        tryMount dev = do
            putStr $ "mounting " ++ dev ++ "1..."
            (_, _, _, p) <- createProcess $ cmd argExtra (dev ++ "1")
            exitStatus <- waitForProcess p
            when (exitStatus == ExitSuccess) $ do
                putStrLn $ "OK\nUSB device " ++ dev ++ "1 (" ++ fsys ++ ") mounted at /mnt/usb"
                exitWith ExitSuccess
            putStr "SKIP\n"

getDevices :: IO [String]
getDevices = do
    (_, sout, _, p) <- createProcess cmd
    devs <- case sout of
        Just h -> hGetContents h
        Nothing -> return []
    _ <- waitForProcess p
    let devs' = filter (not . any isDigit) . words $ devs
    putStrLn "discovered devices:"
    mapM_ (\d -> putStrLn $ "    " ++ d) devs'
    when (null devs) $ do
        errMsg $ "cannot find any sdX devices under /dev"
        exitWith (ExitFailure 1)
    return devs'
    where
        cmd = CreateProcess
            { cmdspec = ShellCommand ("ls /dev/sd*")
            , cwd = Nothing
            , env = Nothing
            , std_in = CreatePipe
            , std_out = CreatePipe
            , std_err = Inherit
            , close_fds = False
            }

errMsg :: String -> IO ()
errMsg msg = hPutStrLn stderr $ "error: " ++ msg
