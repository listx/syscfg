module Main where

-- compile with `ghc -o poke poke.hs'

-- for random numbers
import System.Random

-- for hSetBuffering
import IO

-- for hSetEcho
import System.IO

keys =  ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"] ++ -- 26
        ["A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z"] ++ -- 52
        ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0"] ++ -- 62
        ["`", "~", "!", "@", "#", "$", "%", "^", "&", "*", "(", ")", "_", "-", "+", "=", "[", "]", "{", "}", "\\", "|", ";", ":", "'", "\"", ",", ".", "<", ">", "/", "?"]

puts = putStrLn
put = putStr

showrand :: IO ()
showrand = do
    key <- getChar
    r <- getStdRandom $ randomR (0,(length keys) - 1)
    case key of
        'q' -> puts "\npoke: exiting...\n"
        'j' -> put $ keys!!((mod r 10) + 52)
        'k' -> put $ keys!!((mod r 32) + 62)
        'l' -> put $ keys!!(mod r 52)
        _   -> put $ keys!!r
    if key /= 'q' then showrand else return ()

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering -- disable buffering from STDIN
    hSetBuffering stdout NoBuffering -- disable buffering from STDOUT
    hSetEcho stdin False -- disable terminal echo
    puts "\npoke: 'q'     quit"
    puts "      'j'     number"
    puts "      'k'     punctuation"
    puts "      'l'     letter"
    puts "      else    any\n"
    showrand -- enter loop
