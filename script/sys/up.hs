import Control.Concurrent
import Control.Monad (forM, when)
import Data.List (zip4, unzip4)
import System.Exit
import System.IO
import System.Process
import System

_LAN_IP :: String
_LAN_IP = "192.168.0.255"

-- list of nodes on the LAN that support wake-on-lan (all hardware addresses
-- are for the wired ethernet connections)
_NODES :: [(String, String)]
_NODES =
	[ ("k0", "00:04:4B:02:51:47")
	, ("k1", "00:23:26:5C:07:37")
	, ("k2", "00:12:3F:05:85:FE")
	, ("ocean", "00:50:8D:BC:9B:72")
	]

main :: IO ()
main = do
	hSetBuffering stdin NoBuffering
	hSetBuffering stdout NoBuffering
	hSetBuffering stderr NoBuffering
	hSetEcho stdin False
	args <- getArgs
	-- only get the arguments that could be valid choices for systems
	let
		-- sanitize input (so that we only count valid arguments in the "when"
		-- function below)
		args' = filter (\a -> elem a (map show $ take (length _NODES) [(0::Integer)..])) args
		statusesForced = zip4 (map show [(0::Integer)..])
			(map fst _NODES)
			(map snd _NODES)
			(repeat True)
	when (not $ null args')
		$ mapM_ wakeUp (filter (\(n, _, _, _) -> elem n args') statusesForced)
		>> exitWith ExitSuccess
	putStrLn "Checking WOL-compliant LAN nodes...\n"
	onlines <- forkIOs (map getNodeStatus _NODES)
	let statuses = zip4 (map show [(0::Integer)..]) (map fst _NODES) (map snd _NODES) onlines
	mapM_ (putStrLn . showStatus) statuses
	putStrLn ""
	putStrLn "Choose system to wake (q to exit)"
	wakeNodes statuses args'

wakeNodes :: [(String, String, String, Bool)] -> [String] -> IO ()
wakeNodes statuses systems
	| not $ null systems = mapM_ (wakeUp . nodeInfo)
		$ filter (isOffline statuses) systems
	| otherwise = tryKey =<< getChar
	where
	tryKey :: Char -> IO ()
	tryKey key = do
		if elem [key] nums
			then do
				if isOffline statuses [key]
					then wakeUp (nodeInfo [key]) >> wakeNodes statuses systems
					else wakeNodes statuses systems
			else case key of
				'q' -> return ()
				_ -> wakeNodes statuses systems
	nodeInfo k = head . filter (\(n, _, _, _) -> n == k) $ statuses
	(nums, _, _, _) = unzip4 statuses

-- parse the data structure and see if a node was detected as being offline
isOffline :: [(String, String, String, Bool)] -> String -> Bool
isOffline statuses sys = (\(_, _, _, b) -> not b)
	. head
	. filter (\(n, _, _, _) -> n == sys)
	$ statuses

-- return True if node is online
getNodeStatus :: (String, String) -> IO Bool
getNodeStatus (link, _) = do
	(_, _, _, p) <- spawnCmd
		("ping -c 1 -W 1 " ++ link ++ " >/dev/null 2>&1") True
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

spawnCmd
	:: String
	-> Bool
	-> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
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
showStatus (num, link, _, online) = "  ("
	++ num
	++ ") "
	++ link
	++ " "
	++ (if online then cGreen "ONLINE" else cRed "OFFLINE")

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
