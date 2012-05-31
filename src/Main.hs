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
import Data.Time.Clock (UTCTime, NominalDiffTime, addUTCTime)
import qualified Data.Time.Clock as UTC
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format
import System.Environment (getArgs)
import System.IO
import System.Locale (defaultTimeLocale)
import Text.Printf (printf)

import Network.NTP
import Data.NTP (Time(..), fromTime, toSeconds)
import Win32Compat (getNTPConf)

import Data.IORef
import Control.Concurrent (forkIO)

import Data.Aeson
import Data.Aeson.Encode

import Network (HostName, withSocketsDo)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import qualified Data.Text.Lazy as LT
import Blaze.ByteString.Builder.Char.Utf8 (fromLazyText)
import Data.Text.Lazy.Builder (toLazyText)

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
            . map (drop 1 . T.words)
            . filter ("server" `T.isPrefixOf`)
            . map T.stripStart
            . T.lines

app :: IORef ServiceState -> Application
app ioref req = case (requestMethod req, pathInfo req) of
    ("GET", ["data"])        -> getData maxBound
    ("GET", ["data", n])     -> getData (either error fst (T.decimal n))
    ("GET", ["favicon.ico"]) -> return (responseLazyText status404 [] "")
    ("GET", [])              -> static req { pathInfo = ["index.html"] }
    _                        -> static req
  where
    static = staticApp defaultWebAppSettings

    getData n = do
        state <- liftIO (readIORef ioref)
        return $ responseJSON
            status200
            [("Content-Type", "application/json")
            ,("Server", "ntpmon/0.5")]
            $ (takeData n state)

responseLazyText :: Status -> ResponseHeaders -> LT.Text -> Response
responseLazyText s h t = ResponseBuilder s h (fromLazyText t)

responseJSON :: ToJSON a => Status -> ResponseHeaders -> a -> Response
responseJSON s h x = responseLazyText s h (enc x)
  where
    enc = toLazyText . fromValue . toJSON

data ServerData = ServerData String (U.Vector Time) (U.Vector Double)

instance ToJSON ServerData where
    toJSON (ServerData name ts os) = object [
          "name"    .= name
        , "times"   .= toJSON utcs
        , "offsets" .= toJSON os
        ]
      where
        utcs = V.map fromTime (V.convert ts) :: V.Vector UTCTime

takeData :: Int -> ServiceState -> [ServerData]
takeData _ (ServiceState Nothing _)            = []
takeData n (ServiceState (Just clock) servers) = map (takeServerData n clock) servers

takeServerData :: Int -> Clock -> Server -> ServerData
takeServerData n clock svr = ServerData (svrName svr) times offsets
  where
    samples = U.take n (svrRawSamples svr)
    times   = U.map (localTime clock) samples
    offsets = U.map (toSeconds . offset clock) samples

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

    time <- liftIO UTC.getCurrentTime
    monitorLoop ioref ref ss time
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

sampleInterval :: NominalDiffTime
sampleInterval = realToFrac (1 / samplesPerSecond)

monitorLoop :: IORef ServiceState -> Server -> [Server] -> UTCTime -> NTP ()
monitorLoop ioref ref ss transmitTime = do
    currentTime <- liftIO UTC.getCurrentTime

    -- send requests to servers (if required)
    transmitTime' <- if transmitTime > currentTime
                     then return transmitTime
                     else do
                        mapM transmit (ref:ss)
                        return (sampleInterval `addUTCTime` currentTime)

    -- update any servers which received replies
    -- TODO: pass back flag to say we updated the list
    servers'@(ref':ss') <- updateServers (ref:ss)

    -- sync clock with reference server
    clock <- syncClockWith ref'

    -- update state for web service
    liftIO $ writeIORef ioref ServiceState {
               svcClock   = Just (fst clock)
             , svcServers = servers'
             }

    -- liftIO $ _writeSamples clock (ref':ss') off

    -- give up our CPU time for the next 50ms
    liftIO $ threadDelay 50000

    monitorLoop ioref ref' ss' transmitTime'

syncClockWith :: Server -> NTP (Clock, Seconds)
syncClockWith server = do
    ntp@NTPData{..} <- get
    let clock = adjustClock server ntpClock
    put ntp { ntpClock = fst clock }
    return clock

_writeSamples :: Clock -> [Server] -> Seconds -> IO ()
_writeSamples clock servers off = do
    utc <- fromTime <$> getCurrentTime clock

    let utcTime  = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" utc
        unixTime = (init . show) (utcTimeToPOSIXSeconds utc)
        index    = [unixTime, utcTime]

    putStrLn (intercalate "," (index ++ fields))
  where
    fields = [offMs] ++ offsets ++ delays ++ errors ++ [freq]

    offMs = printf "%.9f" (off * 1000)
    freq = (show . clockFrequency) clock

    samples = map (U.head . svrRawSamples) servers
    offsets = map (showMilli . toSeconds . offset clock) samples
    delays  = map (showMilli . fromDiff clock . roundtrip) samples
    errors  = map (showMilli . (/10) . uncurry (currentError clock)) (zip servers samples)

showMilli :: Seconds -> String
showMilli t = printf "%.4f" ms
  where
    ms = (1000 :: Double) * (realToFrac t)
