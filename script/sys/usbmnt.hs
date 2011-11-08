{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Main where

import qualified Data.ByteString.Lazy as BL
import System.Console.CmdArgs.Implicit
import System.IO
import System.Environment
import System.Exit
import System.Process
import Text.Parsec.Char hiding (upper)
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.String
import qualified Text.Parsec.ByteString.Lazy as PB
import qualified Text.Parsec.Token as PT
import Text.Parsec.Language (emptyDef)
import Control.Monad.Identity

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
        , "The default behavior without any options is to try to mount a USB device."
            ++ " Here, `device' means a device under the /dev directory, and in our context, is actually a file system partition."
            ++ " Many USB drives have only a single partition, in which case the term `device' means both the USB drive and the single partition it has."
        , ""
        , "Also, allowing the $USER to execute the mount and umount commands with sudo privileges (sudo visudo) will make things less clunky."
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
_PROGRAM_VERSION = "0.1.0"
_PROGRAM_INFO = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION
_PROGRAM_DESC = "mount/unmount USB device(s)"
_COPYRIGHT = "(C) Linus Arver 2011"

data BlockDevice = BlockDevice
    { shortname :: String
    , uuid :: UUID
    , fsys :: String
    , mountPoint :: MountPoint
    } deriving (Eq)

data MountPoint
    = MPath { path :: FilePath }
    | Swap
    | Unmounted
    | UnknownBlkidVal
    deriving (Eq)

instance Show BlockDevice where
    show BlockDevice{..} = unwords
        [ shortname
        , fsys
        , uuid
        , show mountPoint
        ]

instance Show MountPoint where
    show (MPath path) = path
    show Swap = "Swap"
    show Unmounted = "Unmounted"
    show UnknownBlkidVal = "UnknownBlkidVal"

blockdeviceDefault :: BlockDevice
blockdeviceDefault = BlockDevice
    { shortname = ""
    , uuid = ""
    , fsys = ""
    , mountPoint = MPath {path = ""}
    }

data Config = Config
    { fsyss :: [(String, String)]
    , uuids :: [(UUID, String)]
    } deriving (Eq, Show)

_ALPHANUM :: String
_ALPHANUM = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

data Color
    = Red
    | Green
    | Yellow
    | Blue
    deriving (Show, Eq, Enum)

colorize :: Color -> String -> String
colorize c s = "\x1b[1;3" ++ show (fromEnum c + 1) ++ "m" ++ s ++ "\x1b[0m"

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering
    opts <- getOpts
    homeDir <- getEnv "HOME"
    errNo <- argsCheck opts homeDir
    when (errNo > 0) $ exitWith $ ExitFailure errNo
    (devs, takenPaths) <- getDevices opts
    let configLoc = homeDir ++ "/.usbmnt"
    configSrc <- BL.readFile configLoc
    (confErrNo, config) <- parseConfig configSrc configLoc
    when (confErrNo > 0) $ exitWith $ ExitFailure confErrNo
    let mountablePaths = filter (\p -> not $ elem p takenPaths) $ map (\p -> "/mnt/u" ++ show p) [(0::Int)..]
        devsKV = zip (map show [(1::Int)..]) . zip devs $ mountablePaths
    prog opts config devsKV

argsCheck :: Opts -> String -> IO Int
argsCheck Opts{..} homeDir
    | null homeDir = e "could not get environment variable $HOME" 1
    | otherwise = return 0
    where
        e :: String -> Int -> IO Int
        e str num = errMsg str >> return num

prog :: Opts -> Config -> [(String, (BlockDevice, FilePath))] -> IO ()
prog opts@Opts{..} config devsKV
    | discover = do
        putStrLn "all devices:"
        mapM_ (\(_, (d, _)) -> putStrLn $ cshow d) devsKV
    | otherwise = do
        putStrLn (if (unmount || unmount_all)
            then "USB device(s) to unmount:"
            else "USB device(s) to mount:")
        mapM_ (\(n, (d, _)) -> putStrLn $ "    " ++ n ++ ") " ++ show' d) devsKV
        putStrLn ""
        mountMenu opts config devsKV
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

mountMenu :: Opts -> Config -> [(String, (BlockDevice, FilePath))] -> IO ()
mountMenu Opts{..} config devsKV
    | unmount = if length devsKV == 1
        then do
            putStrLn "only 1 USB device to unmount"
            tryMount False config (snd . head $ devsKV) >>= exitWith
        else chooseDev prompt devsKV (tryMount False config)
    | unmount_all = do
        putStrLn "unmounting all USB devices..."
        mapM_ (tryMount False config) (map snd devsKV)
        return ()
    | all_devices = do
        putStrLn "mounting all USB devices..."
        mapM_ (tryMount True config) (map snd devsKV)
        return ()
    | length devsKV == 1 = do
        putStrLn "only 1 USB device to mount"
        tryMount True config (snd . head $ devsKV) >>= exitWith
    | otherwise = chooseDev prompt devsKV (tryMount True config)
    where
        prompt :: String
        prompt = if (unmount || unmount_all)
            then "choose USB device to unmount (q to exit)"
            else "choose USB device to mount (q to exit)"

chooseDev :: String -> [(String, (BlockDevice, FilePath))] -> ((BlockDevice, FilePath) -> IO ExitCode) -> IO ()
chooseDev prompt devsKV func = do
    putStrLn prompt
    key <- getLine
    case lookup key devsKV of
        Just dev -> func dev >>= exitWith
        _ -> case key of
            "q" -> return ()
            _ -> chooseDev prompt devsKV func

tryMount :: Bool -> Config -> (BlockDevice, FilePath) -> IO ExitCode
tryMount mount config@Config{..} (bd@BlockDevice{..}, mp)
    | (null margs) = do
        errMsg $ "UUID " ++ squote uuid ++ " was not found in config file"
        errMsg $ "filesystem " ++ squote fsys ++ " was also not found in config file"
        errMsg $ "supported file systems: " ++ (unwords $ map fst fsyss)
        exitWith (ExitFailure 1)
    | otherwise = do
    when mount $ do
        if (null $ mountArgsUUID config uuid)
            then putStrLn $ "filesystem " ++ squote fsys ++ " found in config file"
            else putStrLn $ "UUID " ++ squote uuid ++ " found in config file"
        putStrLn $ "using these arguments: " ++ squote margs
    putStr $ (if mount then "" else "un")
        ++ "mounting "
        ++ shortname
        ++ " (" ++ fsys ++ ") "
        ++ (if mount then "to " ++ mp else "from " ++ show mountPoint)
        ++ ".."
    (_, _, _, p) <- createProcess $ cmd margs shortname
    exitStatus <- waitForProcess p
    if (exitStatus == ExitSuccess)
        then do
            putStrLn "OK"
            return ExitSuccess
        else do
            putStr "FAILED\n"
            errMsg $ (if mount
                then "mount error (perhaps " ++ squote mp ++ " does not exist)"
                else "unmount error")
            return (ExitFailure 1)
    where
        margs = mountArgs config bd
        cmd arguments devPath = CreateProcess
            { cmdspec = ShellCommand (if mount
                then "sudo mount -t " ++ arguments ++ " " ++ devPath ++ " " ++ mp ++ " &>/dev/null"
                else "sudo umount " ++ show mountPoint)
            , cwd = Nothing
            , env = Nothing
            , std_in = CreatePipe
            , std_out = CreatePipe
            , std_err = Inherit
            , close_fds = False
            }

mountArgs :: Config -> BlockDevice -> String
mountArgs Config{..} BlockDevice{..} = case lookup uuid uuids of
    Just a -> a
    _ -> case lookup fsys fsyss of
        Just a -> a
        _ -> []

mountArgsUUID :: Config -> UUID -> String
mountArgsUUID Config{..} uuid' = case lookup uuid' uuids of
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

-- for parsing the computer-generated output of `sudo blkid -o list'
parserIdentifier :: Parser String
parserIdentifier = many1 $ oneOf $ _ALPHANUM ++ "/-_"

parserWhitespace :: Parser String
parserWhitespace = many1 $ oneOf " \t\n\r"

parserMP :: Parser MountPoint
parserMP =
    try ( do
        a <- oneOf "<("
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
    try ( do
        sname <- parserIdentifier
        _ <- parserWhitespace
        fs <- parserIdentifier
        _ <- parserWhitespace
        _ <- parserIdentifier -- leave out the "label" column, even if it exists
        _ <- parserWhitespace
        mp <- parserMP
        _ <- parserWhitespace
        uid <- parserIdentifier
        eof
        return BlockDevice
           { shortname = sname
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
        return BlockDevice
            { shortname = sname
            , uuid = uid
            , fsys = fs
            , mountPoint = mp
            }
    <?> "5 or 4 fields to parse"

parseBlkid :: String -> IO BlockDevice
parseBlkid src =
    case parse blkidParser "output of `sudo blkid -o list'" src of
        Left parseError -> errMsg (show parseError) >> return blockdeviceDefault
        Right result -> return result

-- we use a LanguageDef so that we can get whitespace/newline parsing for FREE
-- in our .usbmnt file
configDef :: PT.GenLanguageDef BL.ByteString () Identity
configDef = emptyDef
    { PT.commentStart   = ""
    , PT.commentEnd     = ""
    , PT.commentLine    = "#"
    , PT.nestedComments = False
    -- the identStart/identLetter define what a UUID will look like (a
    -- dash-separated hex number)
    , PT.identStart     = oneOf $ ['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F']
    , PT.identLetter    = oneOf $ ['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F'] ++ "-"
    , PT.opStart        = char '.'
    , PT.opLetter       = char '.'
    , PT.reservedOpNames= []
    , PT.reservedNames  = []
    , PT.caseSensitive  = True
    }

-- we call makeTokenParser def and pick out just those we need
lexer :: PT.GenTokenParser BL.ByteString () Identity
lexer = PT.makeTokenParser configDef

p_identifier :: ParsecT BL.ByteString () Identity String
p_identifier = PT.identifier lexer
p_stringLiteral :: ParsecT BL.ByteString () Identity String
p_stringLiteral = PT.stringLiteral lexer
p_whiteSpace :: ParsecT BL.ByteString () Identity ()
p_whiteSpace = PT.whiteSpace lexer
p_braces :: ParsecT BL.ByteString () Identity a -> ParsecT BL.ByteString () Identity a
p_braces = PT.braces lexer
p_commaSep :: ParsecT BL.ByteString () Identity a -> ParsecT BL.ByteString () Identity [a]
p_commaSep = PT.commaSep lexer
p_symbol :: String -> ParsecT BL.ByteString () Identity String
p_symbol = PT.symbol lexer

type UUID = String

assocParser :: PB.Parser String -> PB.Parser (UUID, String)
assocParser keyParser = do
    key <- keyParser
    _ <- many $ oneOf " \t"
    _ <- string "="
    _ <- many $ oneOf " \t"
    mountOpts <- p_stringLiteral
    return (key, mountOpts)
    <?> "a key-value association"

hashParser :: String -> PB.Parser String -> PB.Parser [(String, String)]
hashParser hashName keyParser = do
    _ <- p_symbol hashName
    _ <- p_symbol "="
    a <- p_braces (p_commaSep $ assocParser keyParser)
    return a
    <?> "a " ++ hashName ++ " curly brace block"

configParser :: PB.Parser Config
configParser = do
    p_whiteSpace -- take care of leading whitespace/comments as defined by configDef
    -- parse FSYS_HASH first
    fsyss' <- hashParser "FSYS_HASH" (many1 alphaNum)
    p_whiteSpace
    -- now parse UUID_HASH
    uuids' <- hashParser "UUID_HASH" (p_identifier)
    eof
    return $ Config {fsyss = fsyss', uuids = uuids'}
    <?> "config with FSYS_HASH and UUID_HASH blocks"

parseConfig :: BL.ByteString -> String -> IO (Int, Config)
parseConfig src loc =
    case parse configParser ("config file at " ++ squote loc) src of
        Left parseError -> errMsg (show parseError) >> return (1, Config [] [])
        Right result -> return (0, result)
