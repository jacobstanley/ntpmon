{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get, put)
import Control.Monad (when)
import Data.Maybe (fromJust)
import Data.List (intercalate)
import Data.Time.Clock hiding (getCurrentTime)
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
\Usage: ntp-monitor REFERENCE [SERVER]..\
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

    monitorLoop 0 ref ss
  where
    servers = ref:ss

    headers = [("Unix Time", "Seconds Since 1970"), ("UTC Time", "UTC Time")]
           ++ map (header "Offset" "Microseconds") servers
           ++ map (header "Delay"  "Microseconds") servers
           ++ map (header "Filtered Offset" "Microseconds") servers
           ++ map (header "Filtered Delay"  "Microseconds") servers

    header name unit s = (name ++ " - " ++ svrName s, unit)


monitorLoop :: Int -> Server -> [Server] -> NTP ()
monitorLoop syncCount ref ss = do
    -- update servers
    ref' <- updateServer ref
    ss'  <- mapM updateServer ss

    -- sync clock with reference server
    mclock <- syncClockWith ref'

    -- write samples to csv
    when (syncCount > 2) $ liftIO $
        writeSamples (fromJust mclock) (ref':ss')

    -- sleep 1 sec before we update again
    liftIO (threadDelay 1000000)

    let syncCount' = min 3 (syncCount + maybe 0 (const 1) mclock)

    monitorLoop syncCount' ref' ss'

syncClockWith :: Server -> NTP (Maybe Clock)
syncClockWith server = do
    ntp@NTPData{..} <- get
    let mclock = adjustClock server ntpClock
    case mclock of
        Nothing    -> return ()
        Just clock -> put ntp { ntpClock = clock }
    return mclock

writeSamples :: Clock -> [Server] -> IO ()
writeSamples clock servers = putStrLn (intercalate "," fields)
  where
    fields = [unixTime, utcTime] ++ offsets ++ delays
          ++ filteredOffsets ++ filteredDelays

    svrSamples = map (reify clock) . svrRawSamples

    samples = map (head . svrSamples) servers
    offsets = map (showMicro . offset) samples
    delays  = map (showMicro . delay) samples

    filteredSamples = map (clockFilter . svrSamples) servers
    filteredOffsets = map (maybe "_" showMicro . fmap offset) filteredSamples
    filteredDelays  = map (maybe "_" showMicro . fmap delay) filteredSamples

    utc4     = t4 (head samples)
    utcTime  = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S.%q" utc4
    unixTime = (init . show) (utcTimeToPOSIXSeconds utc4)

showMicro :: NominalDiffTime -> String
showMicro t = printf "%.1f" ms
  where
    ms = (1000000 :: Double) * (realToFrac t)
