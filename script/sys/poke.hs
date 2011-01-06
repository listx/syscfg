module Main where

-- compile with `ghc --make poke' (yes, you can just say `poke' instead of `poke.hs')

import System.Random    -- for random numbers
import IO               -- for random numbers
import System.IO        -- for hSetEcho

-- Data structures (constants)
keysChar = ['a'..'z'] ++ ['A'..'Z']
keysNum = ['0'..'9']
keysPunc = "`~!@#$%^&*()-_=+[{]}\\|;:'\",<.>/? "
keysCharNum = keysChar ++ keysNum
keysAll = keysChar ++ keysNum ++ keysPunc

giveKey :: Char -> Int -> Char
giveKey c n
    | c == 'j'  = extractChar keysNum
    | c == 'k'  = extractChar keysChar
    | c == 'l'  = extractChar keysCharNum
    | c == ';'  = extractChar keysPunc
    | c == '\n' = '\n'
    | otherwise = extractChar keysAll
        where extractChar xs = xs!!(mod n (length xs))

showRandomKey :: IO ()
showRandomKey = getChar >>= handleKey
    where handleKey key =
            if key /= 'q'
                then (getStdRandom $ randomR (0,(length keysAll) - 1)) >>=
                     (\r -> putChar $ giveKey key r) >>
                     showRandomKey -- re-start the loop all over again
                else putStrLn "\nBye!" >>
                     return ()

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering -- disable buffering from STDIN
    hSetBuffering stdout NoBuffering -- disable buffering from STDOUT
    hSetEcho stdin False -- disable terminal echo
    putStrLn "\npoke: 'q'     quit"
    putStrLn "      'j'     number"
    putStrLn "      'k'     letter"
    putStrLn "      'l'     alphanumeric"
    putStrLn "      ';'     punctuation"
    putStrLn "      'ENTER' newline"
    putStrLn "      else    any\n"
    showRandomKey -- enter loop
