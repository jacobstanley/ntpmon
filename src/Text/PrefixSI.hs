module Text.PrefixSI (
      showTime
    , showFreq
    ) where

import Text.Printf (printf)

------------------------------------------------------------------------

-- | Shows a time duration (in seconds) using SI prefixes.
showTime :: Double -> String
showTime = showPrefix "sec" 3

-- | Shows a frequency (in hertz) using SI prefixes.
showFreq :: Double -> String
showFreq = showPrefix "Hz" 3

------------------------------------------------------------------------

showPrefix :: String -> Int -> Double -> String
showPrefix unit dp x | x < micro = format "n" nano
                     | x < milli = format "u" micro
                     | x < 1     = format "m" milli
                     | x < kilo  = format ""  1
                     | x < mega  = format "k" kilo
                     | x < giga  = format "M" mega
                     | otherwise = format "G" giga
  where
    fmt = "%." ++ show dp ++ "f %s" ++ unit
    format prefix size = printf fmt (x / size) prefix

    nano  = 1e-9
    micro = 1e-6
    milli = 1e-3
    kilo = 1e3
    mega = 1e6
    giga = 1e9

