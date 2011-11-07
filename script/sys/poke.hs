module Main where

import IO
import System.Random -- for random numbers
import System.IO -- for hSetEcho
import System
import Data.List (nub)

keysChar, keysNum, keysPunc, keysCharNum, keysAll, keysHex :: String
keysChar = ['a'..'z'] ++ ['A'..'Z']
keysHex = ['a'..'f']
keysNum = ['0'..'9']
keysPunc = "`~!@#$%^&*()-_=+[{]}\\|;:'\",<.>/? "
keysCharNum = keysChar ++ keysNum
keysAll = keysChar ++ keysNum ++ keysPunc

giveKey ::  String -> Char -> Int -> Char
giveKey keysCustom c n = case c of
    'i'  -> extractChar (keysNum ++ keysHex)
    'j'  -> extractChar keysNum
    'k'  -> extractChar keysChar
    'l'  -> extractChar keysCharNum
    ';'  -> extractChar keysPunc
    'h'  -> extractChar (keysCharNum ++ keysCustom)
    '\n' -> '\n'
    _    -> extractChar keysAll
    where
        extractChar xs = xs!!mod n (length xs)

showRandomKey :: String -> IO ()
showRandomKey keysCustom = handleKey =<< getChar
    where
        handleKey key = if key /= 'q'
            then getStdRandom (randomR (0, length (keysAll ++ keysCustom) - 1)) >>=
                 putChar . giveKey keysCustom key >>
                 showRandomKey keysCustom
            else putStrLn "\nBye!" >> return ()

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering -- disable buffering from STDIN
    hSetBuffering stdout NoBuffering -- disable buffering from STDOUT
    hSetEcho stdin False -- disable terminal echo
    as <- getArgs
    let as' = filter (\c -> elem c keysAll) . nub $ unwords as
    putStrLn ""
    mapM_ putStrLn
        [ "poke: 'q'     quit"
        , "      'j'     number"
        , "      'k'     letter"
        , "      'l'     alphanumeric"
        , "      ';'     punctuation"
        , "      'h'     alphanumeric" ++ (if not (null as') then " + " ++ as' else "")
        , "      'i'     hexadecimal"
        , "      'ENTER' newline"
        , "      else    any"
        ]
    putStrLn ""
    showRandomKey as' -- enter loop
