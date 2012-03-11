{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get, put)
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
         withNTP (hPutStrLn stderr) $ do
            (reference:servers) <- concat <$> mapM resolveServers hosts
            monitor reference servers

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

        putStrLn "# Offset* and Delay* are the Offset and Delay values for"
        putStrLn "# updates which were selected for use by NTP Clock Filter"
        putStrLn "# algorithm."

        (putStrLn . intercalate "," . map fst) headers
        (putStrLn . intercalate "," . map snd) headers

    monitorLoop ref ss
  where
    servers = ref:ss

    headers = [("Unix Time", "Seconds Since 1970"), ("UTC Time", "UTC Time")]
           ++ map (header "Offset"  "Microseconds") servers
           ++ map (header "Delay"   "Microseconds") servers
           ++ map (header "Offset*" "Microseconds") servers
           ++ map (header "Delay*"  "Microseconds") servers

    header name unit s = (name ++ " - " ++ svrName s, unit)


monitorLoop :: Server -> [Server] -> NTP ()
monitorLoop ref ss = do
    ref' <- updateServer ref
    ss'  <- mapM updateServer ss

    ntp@NTPData{..} <- get
    let clock = adjustClock ref' ntpClock
    put ntp { ntpClock = clock }

    let servers    = ref':ss'
        svrSamples = map (reify clock) . svrRawSamples

        samples = map (head . svrSamples) servers
        offsets = map (showMicro . offset) samples
        delays  = map (showMicro . delay) samples

        bestSamples = map (best . svrSamples) servers
        bestOffsets = map (maybe "_" showMicro . fmap offset) bestSamples
        bestDelays  = map (maybe "_" showMicro . fmap delay) bestSamples

        utc      = t4 (head samples)
        utcTime  = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S.%q" utc
        unixTime = (init . show) (utcTimeToPOSIXSeconds utc)

        fields = [unixTime, utcTime]
              ++ offsets ++ delays
              ++ bestOffsets ++ bestDelays

        settled = (>= bestWindow) . length . svrSamples

    when (settled ref') $
        liftIO (putStrLn (intercalate "," fields))

    liftIO (threadDelay 1000000)
    monitorLoop ref' ss'

------------------------------------------------------------------------

showMicro :: NominalDiffTime -> String
showMicro t = printf "%.1f" ms
  where
    ms = (1000000 :: Double) * (realToFrac t)
