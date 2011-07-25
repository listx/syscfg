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
    , discover :: Bool
    , no_color :: Bool
    } deriving (Data, Typeable, Show, Eq)

progOpts :: Opts
progOpts = Opts
    { all_devices = def &= help "mount all USB devices"
    , unmount = def &= help "choose a USB device to unmount"
    , unmount_all = def &= name "U" &= help "unmount all USB devices"
    , discover = def &= help "list all mounted/unmounted USB devices"
    , no_color = def &= help "disable colors"
    }
    &= details
        [ "Notes:"
        , ""
        , "The default behavior without any options is to try to mount a USB device. Here, `device' means a device under the /dev directory, and in our context, is actually a file system partition. Many USB drives have only a single partition, in which case the term `device' means both the USB drive and the single partition it has."
        ]

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
_PROGRAM_DESC = "mount/unmount USB device(s)"
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

data Color  = Red
            | Green
            | Yellow
            | Blue
            | CNone
    deriving (Show, Eq)

colorize :: Color -> String -> String
colorize c s = case c of
    Blue -> "\x1b[1;34m" ++ s ++ "\x1b[0m"
    Green -> "\x1b[1;32m" ++ s ++ "\x1b[0m"
    Red -> "\x1b[1;31m" ++ s ++ "\x1b[0m"
    Yellow -> "\x1b[1;33m" ++ s ++ "\x1b[0m"
    _ -> s

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering
    opts <- getOpts
    user <- getEnv "USER"
    errNo <- argsCheck opts user
    when (errNo > 0) $ exitWith $ ExitFailure errNo
    (devs, takenPaths) <- getDevices opts
    let mountablePaths = filter (\p -> not $ elem p takenPaths) $ map (\p -> "/mnt/u" ++ show p) [(0::Int)..]
        devsKV = zip (map show [(1::Int)..]) . zip devs $ mountablePaths
    prog opts user devsKV

argsCheck :: Opts -> String -> IO Int
argsCheck Opts{..} user
    | null user = e "could not get environment variable $USER" 1
    | otherwise = return 0
    where
        e :: String -> Int -> IO Int
        e str num = errMsg str >> return num

prog :: Opts -> String -> [(String, (BlockDevice, FilePath))] -> IO ()
prog opts@Opts{..} user devsKV
    | discover = do
        putStrLn "all devices:"
        mapM_ (\(_, (d, _)) -> putStrLn $ cshow d) devsKV
    | otherwise = do
        putStrLn (if (unmount || unmount_all)
            then "USB device(s) to unmount:"
            else "USB device(s) to mount:")
        mapM_ (\(n, (d, _)) -> putStrLn $ "    " ++ n ++ ") " ++ show' d) devsKV
        putStrLn ""
        mountMenu opts user devsKV
    where
        cshow :: BlockDevice -> String
        cshow b@BlockDevice{..}
            | no_color = show b
            | otherwise = case mountPoint of
                Unmounted -> colorize Green $ show b
                MPath _ -> if not $ null $ getUSBMountPath b
                    then colorize Blue $ show b
                    else show b
                _ -> show b
        show' :: BlockDevice -> String
        show' = if not (unmount || unmount_all)
            then show
            else unwords . init . words . show

mountMenu :: Opts -> String -> [(String, (BlockDevice, FilePath))] -> IO ()
mountMenu Opts{..} user devsKV
    | unmount = if length devsKV == 1
        then do putStrLn "only 1 USB device to unmount"
                tryMount False user (snd . head $ devsKV) >>= exitWith
        else chooseDev prompt user devsKV (tryMount False)
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
    | otherwise = chooseDev prompt  user devsKV (tryMount True)
    where
        prompt :: String
        prompt = if (unmount || unmount_all)
            then "choose USB device to unmount (q to exit)"
            else "choose USB device to mount (q to exit)"

chooseDev :: String -> String -> [(String, (BlockDevice, FilePath))] -> (String -> (BlockDevice, FilePath) -> IO ExitCode) -> IO ()
chooseDev prompt user devsKV func = do
    putStrLn prompt
    key <- getLine
    case lookup key devsKV of
        Just dev -> func user dev >>= exitWith
        _ -> case key of
            "q" -> return ()
            _ -> chooseDev prompt user devsKV func

tryMount :: Bool -> String -> (BlockDevice, FilePath) -> IO ExitCode
tryMount mount user (BlockDevice{..}, mp) = do
    when (null $ mountArgs fsys user) $ do
        errMsg $ "unsupported file system " ++ squote fsys ++ "\nsupported file systems: " ++ (unwords $ map fst (_FILE_SYSTEM_ARGS user))
        exitWith (ExitFailure 1)
    putStr $ (if mount == False then "un" else "")
        ++ "mounting "
        ++ shortname
        ++ " (" ++ fsys ++ ") "
        ++ (if mount == False then "from " ++ show mountPoint else "to " ++ mp)
        ++ ".."
    (_, _, _, p) <- createProcess $ cmd (mountArgs fsys user) shortname
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

_FILE_SYSTEM_ARGS :: String -> [(String, String)]
_FILE_SYSTEM_ARGS user =
    [ ("ext2", "ext2 -o rw,relatime")
    , ("vfat", "vfat -o rw,uid=" ++ user ++ ",gid=" ++ user)
    ]

mountArgs :: String -> String -> String
mountArgs fsys user = case lookup fsys (_FILE_SYSTEM_ARGS user) of
    Just a -> a
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
    when (not discover && null toMount && (not (unmount || unmount_all))) $ do
        errMsg $ "cannot find USB devices to mount"
        exitWith (ExitFailure 1)
    when (not discover && null toUnmount && (unmount || unmount_all)) $ do
        errMsg $ "cannot find USB devices to unmount"
        exitWith (ExitFailure 1)
    return $ formatDevs devs'' toMount toUnmount takenPaths
    where
        formatDevs :: [BlockDevice] -> [BlockDevice] -> [BlockDevice] -> [String] -> ([BlockDevice], [String])
        formatDevs ds m um takenPaths
            | discover = (ds, takenPaths)
            | unmount || unmount_all = (um, takenPaths)
            | otherwise = (m, takenPaths)
        cmdBlkid = CreateProcess
            { cmdspec = ShellCommand ("sudo blkid -o list")
            , cwd = Nothing
            , env = Nothing
            , std_in = CreatePipe
            , std_out = CreatePipe
            , std_err = Inherit
            , close_fds = False
            }

getUSBMountPath :: BlockDevice -> String
getUSBMountPath BlockDevice{..} = case mountPoint of
    MPath str -> if take 6 str == "/mnt/u" && (all (\c -> elem c ['0'..'9']) (drop 6 str))
        then str
        else ""
    _ -> ""

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
