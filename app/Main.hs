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
import           Control.Applicative ((<$>), (<*>))
import           Control.Concurrent (forkIO, threadDelay)
import           Control.Concurrent.STM
import           Control.Monad (when, mzero)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.State (get, put)
import           Data.Aeson
import           Data.Aeson.Encode
import           Data.Aeson.Types (emptyArray)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import           Data.Conduit (ResourceT, ($$))
import           Data.Conduit.Attoparsec (sinkParser)
import           Data.List (intercalate)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as LT
import           Data.Text.Lazy.Builder (toLazyText)
import qualified Data.Text.Lazy.Encoding as LT
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
import           System.IO
import           System.Locale (defaultTimeLocale)
import           Text.Printf (printf)

import           Network.NTP
import           Data.NTP (Time(..), toUTCTime, toSeconds)
import           Win32Compat (getNTPConf)

------------------------------------------------------------------------

main :: IO ()
main = withSocketsDo runService

------------------------------------------------------------------------
-- State

data ServiceState = ServiceState {
      svcClock   :: TVar (Maybe Clock)
    , svcServers :: TVar [Server]
    , svcConfig  :: TVar [ServerConfig]
    }

data ServerConfig = ServerConfig {
      cfgHostName :: HostName
    , cfgMode     :: ServerMode
    }

data ServerMode = Prefer | Normal | NoSelect

initState :: IO ServiceState
initState = do
    config     <- readConfig
    svcClock   <- newTVarIO Nothing
    svcServers <- newTVarIO []
    svcConfig  <- newTVarIO config
    return ServiceState{..}

readConfig :: IO [ServerConfig]
readConfig = servers <$> readNTPConf
  where
    readNTPConf = do
        path <- getNTPConf
        T.readFile path

    mkServer [] = error "readConfig: tried to decode blank server entry"
    mkServer (x:xs)
        | elem "prefer" xs   = cfg { cfgMode = Prefer }
        | elem "noselect" xs = cfg { cfgMode = NoSelect }
        | otherwise          = cfg
      where
        cfg = ServerConfig { cfgHostName = T.unpack x
                           , cfgMode     = Normal }

    servers = map mkServer
            . filter (not . null)
            . map (drop 1 . T.words)
            . filter ("server" `T.isPrefixOf`)
            . map T.stripStart
            . T.lines

------------------------------------------------------------------------
-- Running as a service

runService :: IO ()
runService = do
    state <- initState
    forkIO (runMonitor state)
    let app = withHeaders [("Server", "ntpmon/0.5")] (httpServer state)
    run 30030 app

httpServer :: ServiceState -> Application
httpServer state req = case (requestMethod req, pathInfo req) of
    ("GET",  ["servers"])     -> go $ getServers
    ("PUT",  ["servers"])     -> go $ putServers req
    ("GET",  ["data"])        -> go $ getData maxBound
    ("GET",  ["data", n])     -> go $ getData (either error fst (T.decimal n))
    ("BREW", _)               -> fromStatus status418
    ("GET",  ["favicon.ico"]) -> fromStatus status404
    ("GET",  [])              -> static req { pathInfo = ["index.html"] }
    _                         -> static req
  where
    go f   = f state
    static = staticApp defaultWebAppSettings

------------------------------------------------------------------------
-- /servers

getServers :: ServiceState -> ResourceT IO Response
getServers state = do
    cfg <- readTVarIO' (svcConfig state)
    return $ responseJSON status200 [] (toJSON cfg)

putServers :: Request -> ServiceState -> ResourceT IO Response
putServers req state = do
    value <- requestBody req $$ sinkParser json
    let result = fromJSON value :: Result [ServerConfig]
    case result of
        Error _     -> fromStatus status400
        Success cfg -> do
            liftIO $ atomically $ writeTVar (svcConfig state) cfg
            fromStatus status200

instance ToJSON ServerConfig where
  toJSON s = object [
        "hostName" .= cfgHostName s
      , "mode"     .= cfgMode s
      ]

instance FromJSON ServerConfig where
  parseJSON (Object x) = ServerConfig
      <$> x .: "hostName"
      <*> x .: "mode"
  parseJSON _          = mzero

instance ToJSON ServerMode where
  toJSON Prefer   = String "prefer"
  toJSON Normal   = String "normal"
  toJSON NoSelect = String "noSelect"

instance FromJSON ServerMode where
  parseJSON (String "prefer")   = return Prefer
  parseJSON (String "normal")   = return Normal
  parseJSON (String "noSelect") = return NoSelect
  parseJSON _                   = mzero

------------------------------------------------------------------------
-- /data

getData :: Int -> ServiceState -> ResourceT IO Response
getData n state = do
    body <- atomically' $ do
        clock   <- readTVar (svcClock state)
        servers <- readTVar (svcServers state)
        return (maybe emptyArray (takeData n servers) clock)

    return (responseJSON status200 [] body)

takeData :: Int -> [Server] -> Clock -> Value
takeData n xs clock = toJSON $ map (takeServerData n clock) xs

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
fromStatus s = fromStatus' s msg
  where
    msg = LT.decodeUtf8 (LB.fromChunks [statusMessage s])

fromStatus' :: Monad m => Status -> LT.Text -> m Response
fromStatus' s msg = return (responseLazyText s hdr msg)
  where
    hdr = [("Content-Type", "text/plain")]

responseLazyText :: Status -> ResponseHeaders -> LT.Text -> Response
responseLazyText s h t = ResponseBuilder s h (fromLazyText t)

responseJSON :: Status -> ResponseHeaders -> Value -> Response
responseJSON s hs x = responseLazyText s hs' (enc x)
  where
    hs' = [("Content-Type", "application/json")] ++ hs
    enc = toLazyText . fromValue

------------------------------------------------------------------------

runMonitor :: ServiceState -> IO ()
runMonitor state = do
    cfg <- readTVarIO' (svcConfig state)
    let hosts = map cfgHostName cfg ++ ["localhost"]
    (reference:servers) <- resolve hosts
    withNTP (hPutStrLn stderr) (monitor state reference servers)
  where
    resolve xs = liftIO (concat <$> mapM resolveServers xs)

monitor :: ServiceState -> Server -> [Server] -> NTP ()
monitor state ref ss = do
    liftIO $ do
        hSetBuffering stdout LineBuffering
        (putStrLn . intercalate "," . map fst) headers
        (putStrLn . intercalate "," . map snd) headers

    time <- liftIO UTC.getCurrentTime
    monitorLoop state ref ss time
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

monitorLoop :: ServiceState -> Server -> [Server] -> UTCTime -> NTP ()
monitorLoop state ref ss transmitTime = do
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
        atomically' $ do
            writeTVar (svcClock state)   (Just (ntpClock ntp))
            writeTVar (svcServers state) (map fst servers')

    -- loop again
    monitorLoop state (fst ref') (map fst ss') transmitTime'

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

------------------------------------------------------------------------
-- Lifted versions of STM IO functions

-- | 'STM.atomically' lifted in to 'MonadIO'
atomically' :: MonadIO m => STM a -> m a
atomically' = liftIO . atomically

-- | 'STM.readTVarIO' lifted in to 'MonadIO'
readTVarIO' :: MonadIO m => TVar a -> m a
readTVarIO' = liftIO . readTVarIO
