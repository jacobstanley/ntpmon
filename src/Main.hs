{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get, put)
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
    if length hosts < 1
       then putStr usage
       else do
         withNTP (hPutStrLn stderr) $ do
            (reference:servers) <- resolve hosts
            monitor reference servers
  where
    resolve hosts = liftIO (concat <$> mapM resolveServers hosts)

usage :: String
usage = "ntp-monitor 0.4\n\
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

    monitorLoop ref ss
  where
    servers = ref:ss
    refName = svrHostName ref

    headers = [("Unix Time", "Seconds Since 1970"), ("UTC Time", "UTC Time")]
           ++ map (header "(ms)" (\x -> refName ++ " vs " ++ x)) servers
           ++ map (header "(ms)" (\x -> "Network Delay to " ++ x)) servers
           ++ [ ("High Precision Counter Frequency", "(Hz)") ]
          -- ++ map (header "(ms)" (\x -> refName ++ " vs " ++ x ++ " (filtered)")) servers
          -- ++ map (header "(ms)" (\x -> "Network Delay to " ++ x ++ " (filtered)")) servers

    header unit mkname s = (mkname (svrHostName s), unit)


monitorLoop :: Server -> [Server] -> NTP ()
monitorLoop ref ss = do
    -- update servers
    ref' <- updateServer ref
    ss'  <- mapM updateServer ss

    -- sync clock with reference server
    mclock <- syncClockWith ref'

    case mclock of
        -- we're synchronized
        Just clock -> liftIO $ do
            -- write samples to csv
            writeSamples (fromJust mclock) (ref':ss')
            -- sleep 1s before we update again
            threadDelay 1000000

        -- we're not synchronized
        Nothing -> liftIO $ do
            -- only sleep for 50ms, we need some more samples
            threadDelay 50000


    monitorLoop ref' ss'

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
    fields = [unixTime, utcTime] ++ offsets ++ delays ++ [freq]
    --      ++ filteredOffsets ++ filteredDelays

    svrSamples = map (reify clock) . svrRawSamples

    samples = map (head . svrSamples) servers
    offsets = map (showMilli . offset) samples
    delays  = map (showMilli . delay) samples

    --filteredSamples = map (clockFilter . svrSamples) servers
    --filteredOffsets = map (maybe "_" showMilli . fmap offset) filteredSamples
    --filteredDelays  = map (maybe "_" showMilli . fmap delay) filteredSamples

    utc4     = t4 (head samples)
    utcTime  = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" utc4
    unixTime = (init . show) (utcTimeToPOSIXSeconds utc4)

    freq   = (show . clockFrequency) clock

showMilli :: NominalDiffTime -> String
showMilli t = printf "%.4f" ms
  where
    ms = (1000 :: Double) * (realToFrac t)
