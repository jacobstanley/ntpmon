{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get, put)
import Data.List (intercalate)
-- import Data.Time.Clock hiding (getCurrentTime)
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
           ++ [("Filtered Offset", "(ms)")]
           ++ map (header "(ms)" (\x -> refName ++ " vs " ++ x)) servers
           ++ map (header "(ms)" (\x -> "Network Delay to " ++ x)) servers
           ++ map (header "(ms)" (\x -> refName ++ " vs " ++ x ++ " (error)")) servers
           ++ [ ("High Precision Counter Frequency", "(Hz)") ]
          -- ++ map (header "(ms)" (\x -> refName ++ " vs " ++ x ++ " (filtered)")) servers
          -- ++ map (header "(ms)" (\x -> "Network Delay to " ++ x ++ " (filtered)")) servers

    header unit mkname s = (mkname (svrHostName s), unit)


monitorLoop :: Server -> [Server] -> NTP ()
monitorLoop ref ss = do
    -- send requests to servers
    mapM transmit (ref:ss)

    -- wait for replies
    liftIO (threadDelay 1000000)

    -- update any servers which received replies
    (ref':ss') <- updateServers (ref:ss)

    -- sync clock with reference server
    mclock <- syncClockWith ref'

    -- check if we're synchronized
    liftIO $ case mclock of
        Nothing -> return ()
        Just (clock, off) -> do
            -- we are, so write samples to csv
            writeSamples clock (ref':ss') off

    monitorLoop ref' ss'

syncClockWith :: Server -> NTP (Maybe (Clock, Seconds))
syncClockWith server = do
    ntp@NTPData{..} <- get
    let mclock = adjustClock server ntpClock
    case mclock of
        Nothing         -> return ()
        Just (clock, _) -> put ntp { ntpClock = clock }
    return mclock

writeSamples :: Clock -> [Server] -> Seconds -> IO ()
writeSamples clock servers off = do
    utc <- getCurrentTime clock

    let utcTime  = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" utc
        unixTime = (init . show) (utcTimeToPOSIXSeconds utc)
        index    = [unixTime, utcTime]

    putStrLn (intercalate "," (index ++ fields))
  where
    fields = [offMs] ++ offsets ++ delays ++ errors ++ [freq]

    offMs = printf "%.9f" (off * 1000)
    freq = (show . clockFrequency) clock

    samples = map (head . svrRawSamples) servers
    offsets = map (showMilli . offset clock) samples
    delays  = map (showMilli . fromDiff clock . roundtrip) samples
    errors  = map (showMilli . uncurry (currentError clock)) (zip servers samples)

showMilli :: Seconds -> String
showMilli t = printf "%.4f" ms
  where
    ms = (1000 :: Double) * (realToFrac t)
