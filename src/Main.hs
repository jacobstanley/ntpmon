{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get, put)
import Data.List (intercalate)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format
import System.Environment (getArgs)
import System.IO
import System.Locale (defaultTimeLocale)
import Text.Printf (printf)

import Network.NTP
import Win32Compat (getNTPConf)

import Control.Concurrent (forkIO)
import qualified Data.ByteString.Char8 as B

import Network (HostName, withSocketsDo)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Parse
import Network.Wai.Handler.Warp
import Blaze.ByteString.Builder.ByteString
import qualified Data.Text as T
import qualified Data.Text.IO as T

------------------------------------------------------------------------

main :: IO ()
main = withSocketsDo $ do
    args <- getArgs
    case args of
      []            -> putStr usage
      ["--service"] -> runService
      hosts         -> runMonitor hosts

usage :: String
usage = "NTP Monitor 0.5\n\
\Usage: ntpmon REFERENCE [SERVER]..\
\\n\
\\n  REFERENCE  The NTP server which the other servers will be measured\
\\n             against.\
\\n\
\\n  SERVER     An NTP server to monitor.\
\\n\
\\nNTP servers can be specified using either their hostname or IP address.\
\\n"

------------------------------------------------------------------------
-- Running as a service

runService :: IO ()
runService = do
    hosts <- (++ ["localhost"]) <$> readServers
    forkIO (runMonitor hosts)
    run 30030 app

readServers :: IO [HostName]
readServers = servers <$> readNTPConf
  where
    readNTPConf = do
        path <- getNTPConf
        T.readFile path

    servers = map (T.unpack . head)
            . filter (not . null)
            . filter (notElem "noselect")
            . map (drop 1 . T.words)
            . filter ("server" `T.isPrefixOf`)
            . T.lines

app :: Application
app req = do
  (params, _) <- parseRequestBody lbsBackEnd req
  let r = B.concat $ map (\(x,y) -> B.concat [x,y]) params
  return $ ResponseBuilder
      status200
      [("Content-Type", "text/plain")]
      $ copyByteString r

------------------------------------------------------------------------

runMonitor :: [HostName] -> IO ()
runMonitor hosts = withNTP (hPutStrLn stderr) $ do
    (reference:servers) <- resolve hosts
    monitor reference servers
  where
    resolve xs = liftIO (concat <$> mapM resolveServers xs)

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
    liftIO $ threadDelay $ round (1000000 / samplesPerSecond)

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
    errors  = map (showMilli . (/10) . uncurry (currentError clock)) (zip servers samples)

showMilli :: Seconds -> String
showMilli t = printf "%.4f" ms
  where
    ms = (1000 :: Double) * (realToFrac t)
