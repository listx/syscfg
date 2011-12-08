module Main where

import Control.Monad.State
import Crypto.Random.AESCtr
import Data.Binary (decode)
import qualified Data.ByteString.Lazy as B
import Data.List (nub)
import IO
import System
import System.IO -- for hSetEcho

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

showRandomKey :: String -> StateT AESRNG IO ()
showRandomKey keysCustom = handleKey =<< liftIO getChar
    where
        handleKey key = case key of
            '\n' -> liftIO (putChar '\n') >> showRandomKey keysCustom
            'q' -> (liftIO $ putStrLn "\nBye!") >> return ()
            _ -> mapM_ f [0..(49)::Int] >> (liftIO $ putStrLn []) >> showRandomKey keysCustom
            where
                f _ = liftIO
                    . putChar
                    . giveKey keysCustom key
                    . (\n -> mod n (length (keysAll ++ keysCustom) - 1))
                    =<< aesRandomInt

aesRandomInt :: StateT AESRNG IO Int
aesRandomInt = do
    aesState <- get
    let (bs, aesState') = genRandomBytes aesState 16
    put aesState'
    return (decode $ B.fromChunks [bs])

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
    aesState <- makeSystem -- gather entropy from the system to use as the initial seed
    _ <- runStateT (showRandomKey as') aesState -- enter loop
    return ()
