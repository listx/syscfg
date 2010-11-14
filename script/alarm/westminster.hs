module Main where

import Data.Time (getZonedTime, formatTime)
import System.Locale (defaultTimeLocale)
import System.Cmd (rawSystem)
import System.Exit

main :: IO ExitCode
main = do
    now <- getZonedTime
    let hour = formatTime defaultTimeLocale "%I" now in
        rawSystem "timidity" ["--volume", "800", "~/syscfg/sys/sound/" ++ hour ++ "-westminster-FX3_ReverbMax.mid"]
