{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

--module Main (main) where
module Main where

import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate)
import Data.Time.Clock
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format
import System.Environment (getArgs)
import System.IO
import System.Locale (defaultTimeLocale)
import Text.Printf (printf)

import Network.NTP

------------------------------------------------------------------------

main :: IO ()
main = do
    hosts <- getArgs
    if length hosts < 2
       then putStr usage
       else do
         (reference:servers) <- concat <$> mapM resolveServers hosts
         withNTP (hPutStrLn stderr)
                 (monitor reference servers)

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

    let refRecords = map record (svrSamples ref')

    when (length refRecords >= 3) $ liftIO $ do
        let (i0, utc0)   = refRecords !! 1 -- 2nd latest record
            (i1, utc1)   = refRecords !! 0 -- latest record
            deltaCounter = (fromIntegral (i1 - i0) / (utc1 `diffUTCTime` utc0))
                         / 1000000.0
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
calcOffset x y = (`diffUTCTime` t) <$> (timeAt i y)
  where
    (i, t) = record (lastSample x)

lastSample :: Server -> RawSample
lastSample = head . svrSamples

timeAt :: ClockIndex -> Server -> Maybe UTCTime
timeAt i Server{..} =
    if null s2 || null s1
    then Nothing
    else Just (interp i (head s1) (last s2))
  where
    (s2, s1) = span ((> i) . fst) (map record svrSamples)

interp :: ClockIndex -> Record -> Record -> UTCTime
interp i (i0, t0) (i1, t1) = lerpUTC alpha t0 t1
  where
    alpha = fromIntegral (i - i0) / fromIntegral (i1 - i0)

lerpUTC :: NominalDiffTime -> UTCTime -> UTCTime -> UTCTime
lerpUTC alpha t0 t1 = ((t1 `diffUTCTime` t0) * alpha) `addUTCTime` t0

------------------------------------------------------------------------

showMilli :: NominalDiffTime -> String
showMilli t = printf "%.4f" ms
  where
    ms = (1000 :: Double) * (realToFrac t)
