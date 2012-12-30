module Main where

import Control.Monad.State
import Data.List (nub)
import System.Environment (getArgs)
import System.IO
import System.Random

keysChar, keysNum, keysPunc, keysCharNum, keysAll, keysHex :: String
keysChar = ['a'..'z'] ++ ['A'..'Z']
keysHex = ['a'..'f']
keysNum = ['0'..'9']
keysPunc = "`~!@#$%^&*()-_=+[{]}\\|;:'\",<.>/? "
keysCharNum = keysChar ++ keysNum
keysAll = keysChar ++ keysNum ++ keysPunc

giveKey ::  String -> Char -> Int -> Char
giveKey keysCustom c n = extractChar $ case c of
	'i'  -> (keysNum ++ keysHex)
	'j'  -> keysNum
	'k'  -> keysChar
	'l'  -> keysCharNum
	';'  -> keysPunc
	'h'  -> (keysCharNum ++ keysCustom)
	'\n' -> ['\n']
	_ -> keysAll
	where
	extractChar xs = xs!!mod n (length xs)

showRandomKey :: String -> IO ()
showRandomKey keysCustom = handleKey =<< liftIO getChar
	where
	handleKey key = case key of
		'\n' -> putChar '\n' >> showRandomKey keysCustom
		'q' -> putStrLn "\nBye!" >> return ()
		_ -> mapM_ f [0..(49)::Int]
			>> (liftIO $ putStrLn [])
			>> showRandomKey keysCustom
		where
		f _ = putChar
			. giveKey keysCustom key
			. (\n -> mod n (length (keysAll ++ keysCustom) - 1))
			=<< randomIO

main :: IO ()
main = do
	hSetBuffering stdin NoBuffering -- disable buffering from STDIN
	hSetBuffering stdout NoBuffering -- disable buffering from STDOUT
	hSetEcho stdin False -- disable terminal echo
	as <- getArgs
	let as' = filter (\c -> elem c keysAll) . nub $ unwords as
	mapM_ putStrLn
		[ []
		, "poke: 'q'     quit"
		, "      'j'     number"
		, "      'k'     letter"
		, "      'l'     alphanumeric"
		, "      ';'     punctuation"
		, "      'h'     alphanumeric"
			++ (if null as' then [] else " + " ++ as')
		, "      'i'     hexadecimal"
		, "      'ENTER' newline"
		, "      else    any"
		, []
		]
	-- gather entropy from the system to use as the initial seed
	showRandomKey as' -- enter loop
