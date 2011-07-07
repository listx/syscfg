import Control.Concurrent
import Control.Monad (forM)
import Data.List (zip4, unzip4)
import System.Exit
import System.IO
import System.Process

_LAN_IP :: String
_LAN_IP = "192.168.0.255"

-- list of nodes on the LAN that support wake-on-lan
_NODES :: [(String, String)]
_NODES =
    [ ("exelion", "00:04:4B:02:51:47")
    , ("aether.e", "00:23:26:5C:07:37")
    , ("luxion.e", "00:12:3F:05:85:FE")
    , ("ocean", "00:50:8D:BC:9B:72")
    ]

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering
    hSetEcho stdin False
    putStrLn "Checking WOL-compliant LAN nodes...\n"
    onlines <- forkIOs (map getNodeStatus _NODES)
    let statuses = zip4 (map show [(1::Integer)..]) (map fst _NODES) (map snd _NODES) onlines
    mapM_ (putStrLn . showStatus) statuses
    putStrLn ""
    putStrLn "Choose system to wake (q to exit)"
    chooseNode statuses

chooseNode :: [(String, String, String, Bool)] -> IO ()
chooseNode statuses = do
    key <- getChar
    if elem [key] nums
        then do
            let nodeInfo@(_, _, _, online) = head . filter (\(n, _, _, _) -> n == [key]) $ statuses
            if not online
                then wakeUp nodeInfo >> chooseNode statuses
                else chooseNode statuses
        else case key of
            'q' -> return ()
            _ -> chooseNode statuses
    where
        (nums, _, _, _) = unzip4 statuses

-- return True if node is online
getNodeStatus :: (String, String) -> IO Bool
getNodeStatus (link, _) = do
    (_, _, _, p) <- spawnCmd ("ping -c 1 -W 1 " ++ link ++ " >/dev/null 2>&1") True
    -- compile with -threaded in order to make waitForProcess non-blocking
    exitCode <- waitForProcess p
    if exitCode == ExitSuccess
        then return True
        else return False

wakeUp :: (String, String, String, Bool) -> IO ()
wakeUp node@(_, _, mac, _) = do
    putStrLn $ "Waking " ++ showNode node ++ "..."
    (_, _, _, p) <- spawnCmd ("wol -i " ++ _LAN_IP ++ " " ++ mac) True
    -- compile with -threaded in order to make waitForProcess non-blocking
    _ <- waitForProcess p
    return ()

spawnCmd :: String -> Bool -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
spawnCmd str quiet = createProcess CreateProcess
    { cmdspec = ShellCommand str
    , cwd = Nothing
    , env = Nothing
    , std_in = if quiet then Inherit else CreatePipe
    , std_out = if quiet then Inherit else CreatePipe
    , std_err = if quiet then Inherit else CreatePipe
    , close_fds = True
    }

-- concurrent mapM; originally from http://hpaste.org/44348/concurrent_mapm
forkIOs :: [IO a] -> IO [a]
forkIOs xs = do
    ms <- forM xs $ \x -> do
        m <- newEmptyMVar
        _ <- forkIO $ putMVar m =<< x
        return m
    mapM takeMVar ms

showStatus :: (String, String, String, Bool) -> String
showStatus (num, link, _, online) =
    "  (" ++ num ++ ") " ++ link ++ " " ++ (if online then cGreen "ONLINE" else cRed "OFFLINE")

showNode :: (String, String, String, Bool) -> String
showNode (_, link, _, _) = cBlue link

cNone :: String
cNone = "\x1b[0m"

cRed :: String -> String
cRed s = "\x1b[1;31m" ++ s ++ cNone

cGreen :: String -> String
cGreen s = "\x1b[1;32m" ++ s ++ cNone

cBlue :: String -> String
cBlue s = "\x1b[1;34m" ++ s ++ cNone
