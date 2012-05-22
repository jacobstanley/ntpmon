{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main (main) where

import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get, put)
import Data.List (intercalate)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format
import System.Environment (getArgs)
import System.IO
import System.Locale (defaultTimeLocale)
import Text.Printf (printf)

import Network.NTP
import Win32Compat (getNTPConf)

import Data.IORef
import Control.Concurrent (forkIO)

import Data.Aeson

import Data.Data (Data, Typeable)
import Network.Socket (SockAddr(..), PortNumber(..))

import Network (HostName, withSocketsDo)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp
import Blaze.ByteString.Builder.ByteString
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T

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
-- State

data ServiceState = ServiceState {
      svcClock   :: Maybe Clock
    , svcServers :: [Server]
    }

deriving instance Typeable Server
deriving instance Typeable Sample
deriving instance Data SockAddr
deriving instance Data PortNumber
deriving instance Data Sample
deriving instance Data Server

emptyServiceState :: ServiceState
emptyServiceState = ServiceState Nothing []

------------------------------------------------------------------------
-- Running as a service

runService :: IO ()
runService = do
    hosts <- (++ ["localhost"]) <$> readNTPConfServers
    ioref <- newIORef emptyServiceState
    forkIO (runMonitor' hosts ioref)
    run 30030 (app ioref)

readNTPConfServers :: IO [HostName]
readNTPConfServers = servers <$> readNTPConf
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

app :: IORef ServiceState -> Application
app ioref req = case (requestMethod req, pathInfo req) of
    ("GET", ["data"])        -> getData maxBound
    ("GET", ["data", n])     -> getData (either error fst (T.decimal n))
    ("GET", ["favicon.ico"]) -> return (ResponseBuilder status404 [] (copyLazyByteString ""))
    ("GET", [])              -> static req { pathInfo = ["index.html"] }
    _                        -> static req
  where
    static = staticApp defaultWebAppSettings

    getData n = do
        state <- liftIO (readIORef ioref)
        return $ ResponseBuilder
            status200
            [("Content-Type", "application/json")
            ,("Server", "ntpmon/0.5")]
            $ copyLazyByteString $ encode $ map (limit n) (allServerData state)

    limit n (ServerData name xs) = ServerData name (take n xs)

data ServerData = ServerData String [(UTCTime, Double)]

instance ToJSON ServerData where
    toJSON (ServerData name xs) = object [
          "name"    .= name
        , "times"   .= toJSON (map fst xs)
        , "offsets" .= toJSON (map snd xs)
        ]

allServerData :: ServiceState -> [ServerData]
allServerData (ServiceState Nothing _)            = []
allServerData (ServiceState (Just clock) servers) = map (serverData clock) servers

serverData :: Clock -> Server -> ServerData
serverData clock svr = ServerData (svrName svr) (zip times offsets)
  where
    samples = svrRawSamples svr
    times   = map (localTime clock) samples
    offsets = map (offset clock) samples

------------------------------------------------------------------------

runMonitor :: [HostName] -> IO ()
runMonitor hosts = newIORef emptyServiceState >>= runMonitor' hosts

runMonitor' :: [HostName] -> IORef ServiceState -> IO ()
runMonitor' hosts ioref = withNTP (hPutStrLn stderr) $ do
    (reference:servers) <- resolve hosts
    monitor ioref reference servers
  where
    resolve xs = liftIO (concat <$> mapM resolveServers xs)

monitor :: IORef ServiceState -> Server -> [Server] -> NTP ()
monitor ioref ref ss = do
    liftIO $ do
        hSetBuffering stdout LineBuffering
        (putStrLn . intercalate "," . map fst) headers
        (putStrLn . intercalate "," . map snd) headers

    monitorLoop ioref ref ss
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


monitorLoop :: IORef ServiceState -> Server -> [Server] -> NTP ()
monitorLoop ioref ref ss = do
    -- send requests to servers
    mapM transmit (ref:ss)

    -- wait for replies
    liftIO $ threadDelay $ round (1000000 / samplesPerSecond)

    -- update any servers which received replies
    servers@(ref':ss') <- updateServers (ref:ss)

    -- sync clock with reference server
    mclock <- syncClockWith ref'

    -- update state for web service
    liftIO $ writeIORef ioref ServiceState {
               svcClock   = fmap fst mclock
             , svcServers = servers
             }

    -- check if we're synchronized
    liftIO $ case mclock of
        Nothing -> return ()
        Just (_clock, _off) -> do
            -- we are, so write samples to csv
            --_writeSamples clock (ref':ss') off
            return ()

    monitorLoop ioref ref' ss'

syncClockWith :: Server -> NTP (Maybe (Clock, Seconds))
syncClockWith server = do
    ntp@NTPData{..} <- get
    let mclock = adjustClock server ntpClock
    case mclock of
        Nothing         -> return ()
        Just (clock, _) -> put ntp { ntpClock = clock }
    return mclock

_writeSamples :: Clock -> [Server] -> Seconds -> IO ()
_writeSamples clock servers off = do
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
