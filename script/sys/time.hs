module Main where

-- for all the nice time functions
import Data.Time

-- for monthLength
import Data.Time.Calendar.MonthDay

-- for getting commandline args
import System.Environment

-- for defaultTimeLocale
import System.Locale

ftime :: String -> LocalTime -> String
ftime s t = formatTime defaultTimeLocale s t

days :: String
days = "UMTWRFA"

ydaycount :: Integer -> Integer
ydaycount y
    | isLeapYear y = 366
    | otherwise    = 365

mdaycount :: Integer -> Integer
mdaycount y = toInteger (monthLength (isLeapYear y) (fromIntegral y)::Int)

percent :: Integer -> Integer -> Int
percent n d = truncate $ ((fromInteger n / fromInteger d)::Double) * 100

main :: IO ()
main = do
    argv <- getArgs
    ut <- getCurrentTime
    tz <- getTimeZone ut
    let t = utcToLocalTime tz ut
    let t1 = ftime "%Y%m%d" t
    let t2 = ftime "%H:%M" t
    let y = read $ (ftime "%Y" t)::Integer
    let nth_day_y = read $ ftime "%j" t::Integer
    let nth_day_m = read $ ftime "%e" t::Integer

    -- calculate percentage of year completed
    let yperc = show (percent nth_day_y (ydaycount y))
    -- calculate percentage of month completed
    let mperc = show (percent nth_day_m (mdaycount y))
    --let mperc = percent () (monthcount m)
    let long = t1++"/"++[days!!(read (ftime "%w" t)::Int)]++" "++(ftime "%U" t)++" "++yperc++"/"++mperc++" "++t2
    let short = t1++"/"++[days!!(read (ftime "%w" t)::Int)]++" "++t2
    if length argv > 0
        then case argv!!0 of
                "1" -> putStr short
                _   -> putStr long
        else
            putStr long
