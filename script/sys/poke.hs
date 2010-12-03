module Main where

-- compile with `ghc -o poke poke.hs'

-- for random numbers
import System.Random

-- for hSetBuffering
import IO

-- for hSetEcho
import System.IO

keys = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "`~!@#$%^&*()-_=+[{]}\\|;:'\",<.>/? "

puts = putStrLn
put = putStr

showrand :: IO ()
showrand = do
    key <- getChar
    r <- getStdRandom $ randomR (0,(length keys) - 1)
    case key of
        'j' -> putChar $ keys!!((mod r 10) + 52)
        'k' -> putChar $ keys!!(mod r 52)
        'l' -> putChar $ keys!!(mod r 62)
        ';' -> putChar $ keys!!((mod r 33) + 62)
        '\n' -> putChar '\n'
        'q' -> puts []
        _   -> putChar $ keys!!r
    if key /= 'q' then showrand else return ()

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering -- disable buffering from STDIN
    hSetBuffering stdout NoBuffering -- disable buffering from STDOUT
    hSetEcho stdin False -- disable terminal echo
    puts "\npoke: 'q'     quit"
    puts "      'j'     number"
    puts "      'k'     letter"
    puts "      'l'     alphanumeric"
    puts "      ';'     punctuation"
    puts "      'ENTER' newline"
    puts "      else    any\n"
    showrand -- enter loop
