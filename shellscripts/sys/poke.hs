module Main where

-- compile with `ghc -o poke poke.hs'

-- for random numbers
import System.Random

-- for hSetBuffering
import IO

-- for hSetEcho
import System.IO

keys = [
    "A",
    "B",
    "C",
    "D",
    "E",
    "F",
    "G",
    "H",
    "I",
    "J",
    "K",
    "L",
    "M",
    "N",
    "O",
    "P",
    "Q",
    "R",
    "S",
    "T",
    "U",
    "V",
    "W",
    "X",
    "Y",
    "Z",
    "1",
    "2",
    "3",
    "4",
    "5",
    "6",
    "7",
    "8",
    "9",
    "0",
    "` (backtick)",
    "~ (tilde)",
    "!",
    "@",
    "#",
    "$",
    "%",
    "^ (caret)",
    "&",
    "*",
    "(",
    ")",
    "_ (underscore)",
    "- (hyphen/dash/minus)",
    "+",
    "=",
    "[",
    "]",
    "{",
    "}",
    "\\ (backslash)",
    "| (pipe)",
    "; (semicolon)",
    ": (colon)",
    "' (apostrophe)",
    "\" (double quote)",
    ", (comma)",
    ". (period)",
    "<",
    ">",
    "/ (forward slash)",
    "?"
    ]

puts = putStrLn
put = putStr

showrand :: IO ()
showrand = do
    key <- getChar
    r <- getStdRandom $ randomR (0,(length keys) - 1)
    case key of
        'q' -> puts "\npoke: exiting...\n"
        'j' -> puts $ keys!!((mod r 10) + 26)
        'k' -> puts $ keys!!((mod r 32) + 36)
        'l' -> puts $ keys!!(mod r 26)
        _   -> puts $ keys!!r
    if key /= 'q' then showrand else return ()

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering -- disable buffering from STDIN
    hSetEcho stdin False -- disable terminal echo
    puts "\npoke: 'q'     quit"
    puts "      'j'     number"
    puts "      'k'     punctuation"
    puts "      'l'     letter"
    puts "      else    any\n"
    showrand -- enter loop
