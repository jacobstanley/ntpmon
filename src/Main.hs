{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

--module Main (main) where
module Main where

import           Control.Applicative ((<$>))
import           Control.Concurrent (threadDelay)
import           Control.Monad (when)
import           Control.Monad.IO.Class (liftIO)
import           Data.List (intercalate)
import           Data.Time.Clock
import           Data.Time.Format
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import           System.Environment (getArgs)
import           System.Locale (defaultTimeLocale)
import           System.IO
import           Text.Printf (printf)

import           Network.NTP
import           System.Counter (CounterInfo(..), analyzeCounter)

------------------------------------------------------------------------

main :: IO ()
main = do
    hosts <- getArgs
    if length hosts < 2
       then putStr usage
       else do
         (reference:servers) <- concat <$> mapM resolveServers hosts
         withNTP (monitor reference servers)

usage :: String
usage = "ntp-monitor 0.1\n\
\Usage: ntp-monitor REFERENCE SERVER [SERVER]..\
\\n\
\\n  REFERENCE  The NTP server which the other servers will be measured\
\\n             against.\
\\n\
\\n  SERVER     An NTP server to monitor.\
\\n\
\\nNTP servers can be specified using either their hostname or IP address.\
\\n"

monitor :: Server -> [Server] -> NTP ()
monitor ref ss = do
    liftIO $ do
        hSetBuffering stdout LineBuffering

        hPutStr stderr "Analyzing high performance counter..."
        CounterInfo{..} <- analyzeCounter
        hPutStrLn stderr "done"
        hPutStrLn stderr ("Frequency = " ++ showFreq (fromIntegral cntFrequency))
        hPutStrLn stderr ("Precision = " ++ showTime (fromIntegral cntPrecision / fromIntegral cntFrequency))

        (putStrLn . intercalate "," . map fst) headers
        (putStrLn . intercalate "," . map snd) headers

    monitorLoop ref ss
  where
    headers = [ (svrName ref ++ " - Unix Time", "Seconds Since 1970")
              , (svrName ref ++ " - UTC Time", "UTC Time") ]
           ++ map (\s -> (svrName s ++ " - Offset", "Milliseconds")) ss
           ++ [ ("Counter Frequency", "MHz") ]


monitorLoop :: Server -> [Server] -> NTP ()
monitorLoop ref ss = do
    ref' <- updateServer ref
    ss'  <- mapM updateServer ss

    let refRecords = svrRecords ref'

    when (length refRecords >= 3) $ liftIO $ do
        let (c0, utc0)   = refRecords !! 1 -- 2nd latest record
            (c1, utc1)   = refRecords !! 0 -- latest record
            deltaCounter = (fromIntegral (c1 - c0) / (utc1 `diffUTCTime` utc0))
                         / 1000000.0
                      -- / 1000000000.0
                      -- / (3.0666666666667 * 1000000000.0) -- 3.067 GHz
                      -- / (2.995 * 1000000.0)    -- 2.995 MHz

            offsets   = map (showOffset . calcOffset ref) ss'

            utcTime   = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S.%q" utc1
            unixTime  = (init . show) (utcTimeToPOSIXSeconds utc1)
            clockRate = (init . show) deltaCounter

            fields    = [unixTime, utcTime] ++ offsets ++ [clockRate]

        putStrLn (intercalate "," fields)

    liftIO (threadDelay 1000000)
    monitorLoop ref' ss'
  where
    showOffset = maybe "Unknown" showMilli

------------------------------------------------------------------------

calcOffset :: Server -> Server -> Maybe NominalDiffTime
calcOffset x y = (`diffUTCTime` t) <$> (timeAt c y)
  where
    (c, t) = lastRecord x

lastRecord :: Server -> Record
lastRecord = head . svrRecords

timeAt :: Counter -> Server -> Maybe UTCTime
timeAt c Server{..} =
    if null r2 || null r1
    then Nothing
    else Just (interp c (head r1) (last r2))
  where
    (r2, r1) = span ((> c) . fst) svrRecords

interp :: Counter -> Record -> Record -> UTCTime
interp c (c0, t0) (c1, t1) = lerpUTC alpha t0 t1
  where
    alpha = fromIntegral (c - c0) / fromIntegral (c1 - c0)

lerpUTC :: NominalDiffTime -> UTCTime -> UTCTime -> UTCTime
lerpUTC alpha t0 t1 = ((t1 `diffUTCTime` t0) * alpha) `addUTCTime` t0

------------------------------------------------------------------------

showMilli :: NominalDiffTime -> String
showMilli t = printf "%.4f" ms
  where
    ms = (1000 :: Double) * (realToFrac t)

showTime :: Double -> String
showTime = showPrefix "sec" 3

showFreq :: Double -> String
showFreq = showPrefix "Hz" 3

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
