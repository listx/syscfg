module Main where

import Data.Time
import Data.Time.Calendar.MonthDay (monthLength)
import System.Environment
import System.Locale (defaultTimeLocale)

ftime :: String -> LocalTime -> String
ftime s t = formatTime defaultTimeLocale s t

days :: String
days = "UMTWRFA"

ydaycount :: Integer -> Integer
ydaycount y
	| isLeapYear y = 366
	| otherwise = 365

mdaycount :: Integer -> Integer
mdaycount y = toInteger (monthLength (isLeapYear y) (fromIntegral y)::Int)

percent :: Integer -> Integer -> Int
percent n d = truncate $ ((fromInteger n / fromInteger d)::Double) * 100

main :: IO ()
main = do
	argv <- getArgs
	ut <- getCurrentTime
	tz <- getTimeZone ut
	let
		t = utcToLocalTime tz ut
		t1 = ftime "%Y%m%d" t
		t2 = ftime "%H:%M" t
		y = read $ (ftime "%Y" t)::Integer
		nth_day_y = read $ ftime "%j" t::Integer
		nth_day_m = read $ ftime "%e" t::Integer
		-- calculate percentage of year completed
		yperc = show (percent nth_day_y (ydaycount y))
		-- calculate percentage of month completed
		mperc = show (percent nth_day_m (mdaycount y))
		--let mperc = percent () (monthcount m)
		long = t1
			++ "/"
			++ [days!!(read (ftime "%w" t)::Int)]
			++ " "
			++ (ftime "%U" t)
			++ " "
			++ yperc
			++ "/"
			++ mperc
			++ " "
			++ t2
		short = t1
			++ "/"
			++ [days!!(read (ftime "%w" t)::Int)]
			++ " "
			++ t2
	if length argv > 0
		then if (argv!!0 == "1")
			then putStr short
			else putStr long
		else putStr long
