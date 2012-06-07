{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main (main) where

import           Blaze.ByteString.Builder.Char.Utf8 (fromLazyText)
import           Control.Applicative ((<$>))
import           Control.Concurrent (forkIO)
import           Control.Concurrent (threadDelay)
import           Control.Monad (when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State (get, put)
import           Data.Aeson hiding (json)
import           Data.Aeson.Types (emptyArray)
import           Data.Aeson.Encode
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import           Data.IORef
import           Data.List (intercalate)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import           Data.Text.Lazy.Builder (toLazyText)
import qualified Data.Text.Read as T
import           Data.Time.Clock (UTCTime, NominalDiffTime, addUTCTime)
import qualified Data.Time.Clock as UTC
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import           Data.Time.Format
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import           Network (HostName, withSocketsDo)
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Application.Static
import           Network.Wai.Handler.Warp
import           System.Environment (getArgs)
import           System.IO
import           System.Locale (defaultTimeLocale)
import           Text.Printf (printf)

import           Network.NTP
import           Data.NTP (Time(..), toUTCTime, toSeconds)
import           Win32Compat (getNTPConf)

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
    , svcConfig  :: [ServerConfig]
    }

emptyServiceState :: ServiceState
emptyServiceState = ServiceState Nothing [] []

data ServerConfig = ServerConfig {
      cfgHostName  :: HostName
    , cfgSelection :: Selection
    }

data Selection = Prefer | Normal | NoSelect

------------------------------------------------------------------------
-- Running as a service

runService :: IO ()
runService = do
    config <- readConfig
    let hosts = map cfgHostName config ++ ["localhost"]
    ioref <- newIORef emptyServiceState { svcConfig = config }
    forkIO (runMonitor' hosts ioref)
    let app = withHeaders [("Server", "ntpmon/0.5")] (httpServer ioref)
    run 30030 app

readConfig :: IO [ServerConfig]
readConfig = servers <$> readNTPConf
  where
    readNTPConf = do
        path <- getNTPConf
        T.readFile path

    mkServer [] = error "readConfig: tried to decode blank server entry"
    mkServer (x:xs)
        | elem "prefer" xs   = cfg { cfgSelection = Prefer }
        | elem "noselect" xs = cfg { cfgSelection = NoSelect }
        | otherwise          = cfg
      where
        cfg = ServerConfig { cfgHostName  = T.unpack x
                           , cfgSelection = Normal }

    servers = map mkServer
            . filter (not . null)
            . map (drop 1 . T.words)
            . filter ("server" `T.isPrefixOf`)
            . map T.stripStart
            . T.lines

httpServer :: IORef ServiceState -> Application
httpServer ioref req = case (requestMethod req, pathInfo req) of
    ("GET", ["servers"])     -> withState $ getServers
    ("GET", ["data"])        -> withState $ getData maxBound
    ("GET", ["data", n])     -> withState $ getData (either error fst (T.decimal n))
    ("BREW", _)              -> fromStatus status418
    ("GET", ["favicon.ico"]) -> fromStatus status404
    ("GET", [])              -> static req { pathInfo = ["index.html"] }
    _                        -> static req
  where
    static = staticApp defaultWebAppSettings

    withState action = liftIO (readIORef ioref) >>= return . action

------------------------------------------------------------------------
-- /servers

getServers :: ServiceState -> Response
getServers state = responseJSON status200 [] json
  where
    json = toJSON (map svrJSON servers)
    svrJSON s = object [
          "hostname"  .= cfgHostName s
        , "selection" .= cfgSelection s
        ]

    servers = svcConfig state

instance ToJSON Selection where
  toJSON Prefer   = String "prefer"
  toJSON Normal   = String "normal"
  toJSON NoSelect = String "noselect"

------------------------------------------------------------------------
-- /data

getData :: Int -> ServiceState -> Response
getData n state = responseJSON status200 [] (takeData n state)

takeData :: Int -> ServiceState -> Value
takeData n (ServiceState mclock xs _) = case mclock of
    Nothing    -> emptyArray
    Just clock -> toJSON (map (takeServerData n clock) xs)

takeServerData :: Int -> Clock -> Server -> Value
takeServerData n clock svr = object [
      "name"    .= svrName svr
    , "stratum" .= svrStratum svr
    , "refid"   .= B.unpack (svrReferenceId svr)
    , "times"   .= toJSON utcTimes
    , "offsets" .= toJSON offsets
    , "delays"  .= toJSON delays
    ]
  where
    samples = U.take n (svrRawSamples svr)

    offsets  = U.map (toSeconds . offset clock) samples :: U.Vector Double
    delays   = U.map (toSeconds . networkDelay clock) samples :: U.Vector Double

    times    = U.map (localTime clock) samples :: U.Vector Time
    utcTimes = V.map toUTCTime (V.convert times) :: V.Vector UTCTime

------------------------------------------------------------------------
-- WAI Utils

withHeaders :: ResponseHeaders -> Middleware
withHeaders headers app req = do
    response <- app req
    return $ case response of
        ResponseFile    x hs y z -> ResponseFile    x (hs++headers) y z
        ResponseBuilder x hs y   -> ResponseBuilder x (hs++headers) y
        ResponseSource  x hs y   -> ResponseSource  x (hs++headers) y

fromStatus :: Monad m => Status -> m Response
fromStatus s = return (responseLazyText s hdr msg)
  where
    hdr = [("Content-Type", "text/plain")]
    msg = LT.decodeUtf8 (LB.fromChunks [statusMessage s])

responseLazyText :: Status -> ResponseHeaders -> LT.Text -> Response
responseLazyText s h t = ResponseBuilder s h (fromLazyText t)

responseJSON :: Status -> ResponseHeaders -> Value -> Response
responseJSON s hs x = responseLazyText s hs' (enc x)
  where
    hs' = [("Content-Type", "application/json")] ++ hs
    enc = toLazyText . fromValue

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
    -- give up our CPU time for the next 50ms
    liftIO $ threadDelay 50000

    -- get the current PC clock time
    currentTime <- liftIO UTC.getCurrentTime

    -- send requests to servers (if required)
    transmitTime' <- if transmitTime > currentTime
                     then return transmitTime
                     else do
                        mapM transmit (ref:ss)
                        return (sampleInterval `addUTCTime` currentTime)

    -- update any servers which received replies
    servers'@(ref':ss') <- updateServers (ref:ss)

    -- sync clock with reference server
    when (snd ref') $ do
        ntp <- get
        let clock = adjustClock (fst ref') (ntpClock ntp)
        put ntp { ntpClock = fst clock }

    -- update state for web service
    when (any snd servers') $ do
        ntp <- get
        svc <- liftIO $ readIORef ioref
        liftIO $ writeIORef ioref svc {
                   svcClock   = Just (ntpClock ntp)
                 , svcServers = map fst servers'
                 }

    -- loop again
    monitorLoop ioref (fst ref') (map fst ss') transmitTime'

_writeSamples :: Clock -> [Server] -> Seconds -> IO ()
_writeSamples clock servers off = do
    utc <- toUTCTime <$> getCurrentTime clock

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
