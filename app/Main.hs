{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Main (main) where

import           Blaze.ByteString.Builder.Char.Utf8 (fromLazyText)
import           Control.Applicative ((<$>), (<*>))
import           Control.Concurrent (forkIO, threadDelay)
import qualified Control.Concurrent.STM as STM
import           Control.Concurrent.STM hiding (atomically, readTVarIO)
import           Control.Monad (when, mzero)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.DeepSeq (NFData(..), force)
import           Control.DeepSeq.TH (deriveNFDatas)
import           Data.Aeson
import           Data.Aeson.Encode
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import           Data.Conduit (ResourceT, ($$))
import           Data.Conduit.Attoparsec (sinkParser)
import           Data.Function (on)
import           Data.List (find)
import           Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import           Data.Text.Lazy.Builder (toLazyText)
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text.Read as T
import           Data.Time.Clock (UTCTime, NominalDiffTime, addUTCTime)
import qualified Data.Time.Clock as UTC
import           Data.Time.Format (formatTime)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import           Network (withSocketsDo)
import           Network.Socket (SockAddr(..), PortNumber(..))
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Application.Static hiding (FilePath)
import           Network.Wai.Handler.Warp
import           System.Locale (defaultTimeLocale)
import           System.IO
import           Text.Printf (printf)

import           Network.NTP
import           Network.NTP.Config
import           Network.NTP.ConfigFinder (findConfig)
import           Network.NTP.Types (Time(..), toUTCTime, toSeconds)
import           System.Win32File

------------------------------------------------------------------------

main :: IO ()
main = withSocketsDo runService

------------------------------------------------------------------------
-- State

data ServiceState = ServiceState {
      svcClock0  :: Clock
    , svcServers :: TVar [Server]
    , svcConfig  :: TVar [ServerConfig]
    }

initState :: IO ServiceState
initState = do
    svcClock0  <- initCounterClock (hPutStrLn stderr)
    svcServers <- newTVarIO []
    svcConfig  <- newTVarIO []
    return ServiceState{..}

updateConfig :: ServiceState -> [ServerConfig] -> IO ()
updateConfig state cfg = do
    let hosts = ["localhost"] ++ map cfgHostName cfg
        clock = svcClock0 state
    servers <- catMaybes <$> mapM (resolveServer clock . T.unpack) hosts
    atomically $ do
        writeTVar (svcConfig state) cfg
        modifyTVar' (svcServers state) (force . mergeServers servers)

updateData :: ServiceState -> [Server] -> STM ()
updateData state servers =
    modifyTVar' (svcServers state) (force . flip mergeServers servers)

-- | Returns the server addresses from the first list combined with
-- the server data from the second list.
mergeServers :: [Server] -> [Server] -> [Server]
mergeServers xs ys = zs
  where
    zs = map (findByName ys) xs

-- Finds the server which has a matching address, or if not, simply
-- returns the server used as the search candidate.
findByName :: [Server] -> Server -> Server
findByName ys x = maybe x id (find (nameEq x) ys)
  where
    nameEq = (==) `on` svrHostName

type Master = Server

-- Finds the server which is the reference server for localhost.
-- NOTE: Assumes that the first server in the list is localhost.
findMaster :: [Server] -> Master
findMaster []         = error "findMaster: empty server list"
findMaster [local]    = local
findMaster (local:xs) = maybe local id (find isMaster xs)
  where
    isMaster svr = svrRefAddr local == svrAddr svr

------------------------------------------------------------------------
-- Running as a service

runService :: IO ()
runService = do
    state <- initState
    updateConfig state =<< readConfig =<< findConfig
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
    cfg <- readTVarIO (svcConfig state)
    return $ responseJSON status200 [] (toJSON cfg)

putServers :: Request -> ServiceState -> ResourceT IO Response
putServers req state = do
    value <- requestBody req $$ sinkParser json
    let result = fromJSON value :: Result [ServerConfig]
    case result of
        Error _     -> fromStatus status400
        Success cfg -> do
            liftIO $ do
                writeConfig cfg =<< findConfig
                updateConfig state cfg
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
    body <- atomically $ do
        servers <- readTVar (svcServers state)
        return $ takeData n servers (findMaster servers)

    return (responseJSON status200 [] body)

takeData :: Int -> [Server] -> Master -> Value
takeData n xs master = toJSON $ map (takeServerData n master) xs

takeServerData :: Int -> Master -> Server -> Value
takeServerData n master svr = object [
      "name"     .= svrName svr
    , "isMaster" .= toJSON (svrSockAddr svr == svrSockAddr master)
    , "stratum"  .= svrStratum svr
    , "refid"    .= B.unpack (svrRefId svr)
    , "times"    .= toJSON utcTimes
    , "offsets"  .= toJSON offsets
    , "delays"   .= toJSON delays
    ]
  where
    samples = U.take n (svrRawSamples svr)

    clock    = svrClock master
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
    time <- UTC.getCurrentTime
    file <- newRecordLogger "."
    withNTP (hPutStrLn stderr) (monitor state file time)

monitor :: ServiceState -> RecordLogger -> UTCTime -> NTP ()
monitor state file transmitTime = do
    -- give up our CPU time for the next 50ms
    liftIO $ threadDelay 50000

    -- get the current PC clock time
    currentTime <- liftIO UTC.getCurrentTime

    -- read servers from tvar
    servers <- readTVarIO (svcServers state)

    -- send requests to servers (if required)
    transmitTime' <- if transmitTime > currentTime
                     then return transmitTime
                     else do
                        sendRequests servers
                        liftIO (writeCSV file servers)
                        return (sampleInterval `addUTCTime` currentTime)

    -- update any servers which received replies
    servers' <- updateServers servers

    -- update state for web service
    when (any snd servers') $
        atomically $ updateData state (map fst servers')

    -- loop again
    monitor state file transmitTime'

  where
    -- sendRequests inserts a 5ms delay after each transmission so that
    -- adjacent requests affect each other as little as possible
    sendRequests = mapM (\x -> transmit x >> delay5ms)
    delay5ms = liftIO (threadDelay 5000)

sampleInterval :: NominalDiffTime
sampleInterval = realToFrac (1 / samplesPerSecond)

------------------------------------------------------------------------
-- CSV Logging

writeCSV :: RecordLogger -> [Server] -> IO ()
writeCSV _    [] = return ()
writeCSV file xs = do
    utc <- toUTCTime <$> getCurrentTime clock
    case (csvHeaders xs, csvRecord utc master xs) of
        (Just hdr, Just rec) -> writeRecord file utc hdr rec
        _                    -> return ()
  where
    master = findMaster xs
    clock  = svrClock master

csvRecord :: UTCTime -> Master -> [Server] -> Maybe T.Text
csvRecord utc master xs | empty     = Nothing
                        | otherwise = Just record
  where
    empty = all (U.null . svrRawSamples) xs

    record = T.intercalate "," fields `T.append` "\r\n"
    fields = [time] ++ offsets ++ delays ++ [masterName]

    time       = T.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" utc)
    masterName = T.pack (svrName master)

    samples = map ((U.!? 0) . svrRawSamples) xs
    offsets = map (maybe "" $ showMilli . toSeconds . offset clock) samples
    delays  = map (maybe "" $ showMilli . fromDiff clock . roundtrip) samples
    clock   = svrClock master

csvHeaders :: [Server] -> Maybe T.Text
csvHeaders [] = Nothing
csvHeaders xs = Just $ T.intercalate "," headers `T.append` "\r\n"
  where
    names   = map (T.pack . svrName) xs
    headers = ["UTC Time"]
           ++ map (\x -> T.concat [x, " vs UTC (ms)"]) names
           ++ map (\x -> T.concat ["Network Delay to ", x, " (ms)"]) names
           ++ ["Master"]

showMilli :: Seconds -> T.Text
showMilli t = T.pack (printf "%.4f" ms)
  where
    ms = (1000 :: Double) * (realToFrac t)

------------------------------------------------------------------------
-- Lifted versions of STM IO functions

-- | 'STM.atomically' lifted in to 'MonadIO'
atomically :: MonadIO m => STM a -> m a
atomically = liftIO . STM.atomically

-- | 'STM.readTVarIO' lifted in to 'MonadIO'
readTVarIO :: MonadIO m => TVar a -> m a
readTVarIO = liftIO . STM.readTVarIO

------------------------------------------------------------------------
-- NFData Instances

deriving instance NFData Time
deriving instance NFData PortNumber

instance NFData ByteString
instance U.Unbox a => NFData (U.Vector a)

$(deriveNFDatas [''SockAddr, ''Clock, ''Server])
