{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Main where

import Control.Monad (liftM, when)
import Data.Bits
import Data.List (foldl', nub)
import Numeric
import System.Console.CmdArgs.Implicit
import System.IO
import System.Directory
import System.Exit

data Opts = Opts
	{ sha_hash :: [String]
	, file :: [FilePath]
	} deriving (Data, Typeable, Show, Eq)

progOpts :: Opts
progOpts = Opts
	{ sha_hash = def &= typ "SHA1 HASH" &= help "Additional SHA1 hashes in hexadecimal to XOR into (use this flag once for each additional hash; also, do not prefix the hex with `0x'; e.g., use \"f3\" instead of \"0xf3\"). Leading zeroes are ignored; trailing non-hex characters (as well as non-leading-hex strings) are also ignored."
	, file = [] &= typFile &= help "Read hashes from a file; the expected format of the file is the output of the sha1sum(1) program. You can use this flag multiple times for multiple files. If --file is not used at all, then shaxor expects input from STDIN; thus, you can use it like this: sha1sum FILES | shaxor."
	}
	&= details
		[ "Notes:"
		, ""
		, "  Although designed to read 40-character-long hex strings, shaxor can read in any arbitrarily long hex string."
		]

getOpts :: IO Opts
getOpts = cmdArgs $ progOpts
	&= summary (_PROGRAM_INFO ++ ", " ++ _COPYRIGHT)
	&= program _PROGRAM_NAME
	&= help _PROGRAM_DESC
	&= helpArg [explicit, name "help", name "h"]

_PROGRAM_NAME
	, _PROGRAM_VERSION
	, _PROGRAM_INFO
	, _PROGRAM_DESC
	, _COPYRIGHT :: String
_PROGRAM_NAME = "shaxor"
_PROGRAM_VERSION = "0.0.1"
_PROGRAM_INFO = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION
_PROGRAM_DESC = "binary XOR multiple SHA1 hashes (based on sha1sum(1) format)"
_COPYRIGHT = "(C) Linus Arver 2012"

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

argsCheck :: Opts -> IO Int
argsCheck Opts{..}
	| otherwise = return 0

-- Verify that the --file and --list arguments actually make sense.
filesCheck :: [Bool] -> IO Int
filesCheck fs
	| any (==False) fs = errMsgNum "an argument to --file does not exist" 1
	| otherwise = return 0

main :: IO ()
main = do
	hSetBuffering stdout NoBuffering
	hSetBuffering stderr NoBuffering
	hSetEcho stdin False -- disable terminal echo
	opts@Opts{..} <- getOpts
	errNo <- argsCheck opts
	when (errNo > 0) $ exitWith $ ExitFailure errNo

	hash <- return . shaxor =<< (if null file
		then hGetContents stdin
		else return [])
	fs <- mapM doesFileExist file -- e.g., --file x --file y --file z
	errNo' <- filesCheck fs
	when (errNo' > 0) $ exitWith $ ExitFailure errNo
	prog opts hash file

prog :: Opts -> Integer -> [FilePath] -> IO ()
prog Opts{..} hash files = do
	hashes <- mapM (return . liftM shaxor =<< readFile) files
	let
		hashes2 = map shaxor sha_hash
		hash' = foldl' xor hash (hashes ++ hashes2)
	putStrLn $ showHex hash' []

-- Takes a sha1sum(1) formatted string, and XORs all of the hashes in there.
shaxor :: String -> Integer
shaxor = foldl' xor 0
	. map
		( (\x -> if null x
			then 0
			else fst $ head x)
		. readHex
		)
	. nub
	. filter (not . null)
	. lines

errMsg :: String -> IO ()
errMsg msg = hPutStrLn stderr $ "error: " ++ msg

errMsgNum :: String -> Int -> IO Int
errMsgNum str num = errMsg str >> return num

squote :: String -> String
squote s = "`" ++ s ++ "'"
