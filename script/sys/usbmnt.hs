{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Main where

import System.Console.CmdArgs.Implicit
import System.IO
import System.Environment
import System.Exit
import System.Process

import Text.Parsec.Char hiding (upper)
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.String
import Control.Monad.Identity
--import qualified Text.Show.Pretty as Pr

data Opts = Opts
    { all_devices :: Bool
    , unmount :: Bool
    , unmount_all :: Bool
    } deriving (Data, Typeable, Show, Eq)

progOpts :: Opts
progOpts = Opts
    { all_devices = def &= help "mount all mountable USB devices; default FALSE"
    , unmount = def &= help "choose a USB device to unmount"
    , unmount_all = def &= name "U" &= help "unmount all USB devices"
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

data BlockDevice = BlockDevice
    { shortname :: String
    , uuid :: String
    , fsys :: String
    , mountPoint :: MountPoint
    } deriving (Eq)

data MountPoint = MPath { path :: FilePath }
                | Swap
                | Unmounted
                | UnknownBlkidVal
    deriving (Eq)

instance Show BlockDevice where
    show BlockDevice{..} = unwords  [ shortname
                                    , fsys
                                    , uuid
                                    , show mountPoint
                                    ]

instance Show MountPoint where
    show (MPath path) = path
    show Swap = "Swap"
    show Unmounted = "Unmounted"
    show UnknownBlkidVal = "UnknownBlkidVal"

_BLOCKDEVICE_DEFAULT :: BlockDevice
_BLOCKDEVICE_DEFAULT = BlockDevice
    { shortname = ""
    , uuid = ""
    , fsys = ""
    , mountPoint = MPath {path = ""}
    }

_ALPHANUM :: String
_ALPHANUM = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering
    opts <- getOpts
    user <- getEnv "USER"
    errNo <- argsCheck opts user
    when (errNo > 0) $ exitWith $ ExitFailure errNo
    prog opts user

argsCheck :: Opts -> String -> IO Int
argsCheck Opts{..} user
    | null user = e "could not get environment variable $USER" 1
    | otherwise = return 0
    where
        e :: String -> Int -> IO Int
        e str num = errMsg str >> return num

prog :: Opts -> String -> IO ()
prog opts@Opts{..} user = do
    (devs, takenPaths) <- getDevices opts
    let mountablePaths = filter (\p -> not $ elem p takenPaths) $ map (\p -> "/mnt/u" ++ show p) [(0::Int)..]
        devsKV = zip (map show [(1::Int)..]) . zip devs $ mountablePaths
    putStrLn (if (unmount || unmount_all)
        then "device(s) to unmount:"
        else "mounted device(s):")
    mapM_ (\(n, (d, _)) -> putStrLn $ "    " ++ n ++ ") " ++ show d) devsKV
    putStrLn ""
    mountMenu opts user devsKV

mountMenu :: Opts -> String -> [(String, (BlockDevice, FilePath))] -> IO ()
mountMenu Opts{..} user devsKV
    | unmount = if length devsKV == 1
        then do putStrLn "only 1 USB device to unmount"
                tryMount False user (snd . head $ devsKV) >>= exitWith
        else do putStrLn "choose USB device to unmount (q to exit)"
                chooseDev user devsKV (tryMount False)
    | unmount_all = do
        putStrLn "unmounting all USB devices..."
        mapM_ (tryMount False user) (map snd devsKV)
        return ()
    | all_devices = do
        putStrLn "mounting all USB devices..."
        mapM_ (tryMount True user) (map snd devsKV)
        return ()
    | length devsKV == 1 = do
        putStrLn "only 1 USB device to mount"
        tryMount True user (snd . head $ devsKV) >>= exitWith
    | otherwise = do
        putStrLn "choose USB device to mount (q to exit)"
        chooseDev user devsKV (tryMount True)

chooseDev :: String -> [(String, (BlockDevice, FilePath))] -> (String -> (BlockDevice, FilePath) -> IO ExitCode) -> IO ()
chooseDev user devsKV func = do
    key <- getLine
    case lookup key devsKV of
        Just dev -> func user dev >>= exitWith
        _ -> case key of
            "q" -> return ()
            _ -> putStrLn "invalid input; try again" >> chooseDev user devsKV func

tryMount :: Bool -> String -> (BlockDevice, FilePath) -> IO ExitCode
tryMount mount usr (BlockDevice{..}, mp) = do
    when (null $ mountArgs fsys usr) $ do
        errMsg $ "invalid file system " ++ squote fsys
        exitWith (ExitFailure 1)
    putStr $ (if mount == False then "un" else "")
        ++ "mounting USB device "
        ++ shortname
        ++ " (" ++ fsys ++ ") "
        ++ (if mount == False then "from " ++ show mountPoint else "to " ++ mp)
        ++ ".."
    (_, _, _, p) <- createProcess $ cmd (mountArgs fsys usr) shortname
    exitStatus <- waitForProcess p
    if (exitStatus == ExitSuccess)
        then do putStrLn "OK"
                return ExitSuccess
        else do putStr "FAILED\n"
                errMsg $ "mount error (perhaps " ++ squote mp ++ " does not exist)"
                return (ExitFailure 1)
    where
        cmd arguments devPath = CreateProcess
            { cmdspec = ShellCommand (if mount == False
                then "sudo umount " ++ show mountPoint
                else "sudo mount -t " ++ arguments ++ " " ++ devPath ++ " " ++ mp ++ " &>/dev/null")
            , cwd = Nothing
            , env = Nothing
            , std_in = CreatePipe
            , std_out = CreatePipe
            , std_err = Inherit
            , close_fds = False
            }

mountArgs :: String -> String -> String
mountArgs fsys user = case fsys of
    "ext2" -> "ext2 -o rw,relatime"
    "vfat" -> "vfat -o rw,uid=" ++ user ++ ",gid=" ++ user
    _ -> []

getDevices :: Opts -> IO ([BlockDevice], [String])
getDevices Opts{..} = do
    (_, sout, _, p) <- createProcess cmdBlkid
    devs <- case sout of
        Just h -> hGetContents h
        Nothing -> return []
    _ <- waitForProcess p
    let devs' = (map (unwords . words)) . drop 2 . lines $ devs
    devs'' <- mapM parseBlkid devs'
    let toMount = filter (\BlockDevice{..} -> mountPoint == Unmounted) devs''
        toUnmount = filter (\dev -> not $ null $ getUSBMountPath dev) devs''
        takenPaths = filter (not . null) . map getUSBMountPath $ devs''
    when (null toMount && (not (unmount || unmount_all))) $ do
        errMsg $ "cannot find any mountable devices"
        exitWith (ExitFailure 1)
    when (null toUnmount && (unmount || unmount_all)) $ do
        errMsg $ "cannot find any devices to unmount"
        exitWith (ExitFailure 1)
    if ((unmount || unmount_all) == True)
        then return (toUnmount, takenPaths)
        else return (toMount, takenPaths)
    where
        getUSBMountPath :: BlockDevice -> String
        getUSBMountPath BlockDevice{..} = case mountPoint of
            MPath str -> if take 6 str == "/mnt/u" && (elem (last str) ['0'..'9'])
                then str
                else ""
            _ -> ""
        cmdBlkid = CreateProcess
            { cmdspec = ShellCommand ("sudo blkid -o list")
            , cwd = Nothing
            , env = Nothing
            , std_in = CreatePipe
            , std_out = CreatePipe
            , std_err = Inherit
            , close_fds = False
            }

errMsg :: String -> IO ()
errMsg msg = hPutStrLn stderr $ "error: " ++ msg

squote :: String -> String
squote s = "`" ++ s ++ "'"

-- Parsing
parserIdentifier :: Parser String
parserIdentifier = many1 $ oneOf $ _ALPHANUM ++ "/-_"

parserWhitespace :: Parser String
parserWhitespace = many1 $ oneOf " \t\n\r"

parserMP :: Parser MountPoint
parserMP =
    try (   do  a <- oneOf "<("
                b <- manyTill anyChar (lookAhead $ (oneOf ">)"))
                _ <- oneOf ">)"
                let mp = case a of
                        '<' -> Swap
                        '(' -> case b of
                            "not mounted" -> Unmounted
                            _ -> UnknownBlkidVal
                        _ -> UnknownBlkidVal
                return mp
        )
    <|> (parserIdentifier >>= (\s -> return MPath {path = s}))
    <?> "blkid's mount point description"

blkidParser :: Parser BlockDevice
blkidParser =
    try (   do  sname <- parserIdentifier
                _ <- parserWhitespace
                fs <- parserIdentifier
                _ <- parserWhitespace
                _ <- parserIdentifier -- leave out the "label" column, even if it exists
                _ <- parserWhitespace
                mp <- parserMP
                _ <- parserWhitespace
                uid <- parserIdentifier
                eof
                return BlockDevice  { shortname = sname
                                    , uuid = uid
                                    , fsys = fs
                                    , mountPoint = mp
                                    }
        )
    <|>
    do  sname <- parserIdentifier
        _ <- parserWhitespace
        fs <- parserIdentifier
        _ <- parserWhitespace
        mp <- parserMP
        _ <- parserWhitespace
        uid <- parserIdentifier
        eof
        return BlockDevice  { shortname = sname
                            , uuid = uid
                            , fsys = fs
                            , mountPoint = mp
                            }
    <?> "5 or 4 fields to parse"

parseBlkid :: String -> IO BlockDevice
parseBlkid src =
    case parse blkidParser "output of `sudo blkid -o list'" src of
        Left parseError -> errMsg (show parseError) >> return _BLOCKDEVICE_DEFAULT
        Right result -> return result
