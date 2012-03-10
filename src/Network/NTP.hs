{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.NTP where

import           Control.Applicative ((<$>))
import           Control.Exception (IOException, bracket, handle)
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Serialize
import           Data.Time.Clock (UTCTime, addUTCTime, diffUTCTime)
import qualified Data.Time.Clock as T
import           Data.Word (Word64)
import           Network.Socket hiding (send, sendTo, recv, recvFrom)
import           Network.Socket.ByteString
import           System.Timeout (timeout)

import           Network.NTP.Types hiding (Server)
import           System.Counter
import           Text.PrefixSI

------------------------------------------------------------------------
-- NTP Monad

newtype NTP a = NTP { runNTP :: ReaderT NTPData IO a }
    deriving (Monad, MonadIO, MonadReader NTPData)

data NTPData = NTPData {
      ntpSocket :: Socket
    , ntpClock  :: Clock
    }

type Logger = String -> IO ()

withNTP :: Logger -> NTP a -> IO a
withNTP logger = withSocketsDo . bracket create destroy . runReaderT . runNTP
  where
    create = do
        ntpSocket <- socket AF_INET Datagram defaultProtocol
        ntpClock  <- initCounterClock logger
        logger (show ntpClock)
        return NTPData{..}

    destroy NTPData{..} = sClose ntpSocket

------------------------------------------------------------------------
-- Clock

type ClockIndex = Word64

data Clock = Clock {
      clockTime0      :: UTCTime
    , clockIndex0     :: ClockIndex
    , clockFrequency  :: Word64
    , clockPrecision  :: Word64
    , getCurrentIndex :: IO ClockIndex
    } deriving (Show)

instance Show (IO ClockIndex) where
    show _ = ".."

-- | Initializes a 'Clock' based on the high precision counter.
initCounterClock :: Logger -> IO Clock
initCounterClock logger = do
    logger "Analyzing high precision counter..."

    counter <- analyzeCounter

    let clockFrequency = cntFrequency counter
        clockPrecision = cntPrecision counter
        freq = fromIntegral clockFrequency
        prec = fromIntegral clockPrecision / freq

    logger ("Frequency = " ++ showFreq freq)
    logger ("Precision = " ++ showTime prec)

    let getCurrentIndex = readCounter
    clockTime0  <- T.getCurrentTime
    clockIndex0 <- getCurrentIndex

    return Clock{..}

-- | Gets the current time according to the 'Clock'.
getCurrentTime :: Clock -> IO UTCTime
getCurrentTime clock = clockTime clock <$> getCurrentIndex clock

-- | Gets the time at the given index.
clockTime :: Clock -> ClockIndex -> UTCTime
clockTime Clock{..} index = (diff / freq) `addUTCTime` clockTime0
  where
    diff = fromIntegral (index - clockIndex0)
    freq = fromIntegral clockFrequency

------------------------------------------------------------------------
-- Server

data Server = Server {
      svrHostName :: HostName
    , svrAddress  :: SockAddr
    , svrRecords  :: [Record]
    } deriving (Show)

type Record = (UTCTime, UTCTime)

svrName :: Server -> String
svrName svr | host /= addr = host ++ " (" ++ addr ++ ")"
            | otherwise    = host
  where
    host = svrHostName svr
    addr = (takeWhile (/= ':') . show . svrAddress) svr

------------------------------------------------------------------------

-- | Resolves a list of IP addresses registered for the specified
-- hostname and creates 'Server' instances for each of them.
resolveServers :: HostName -> IO [Server]
resolveServers host =
    map (mkServer . addrAddress) <$> getHostAddrInfo
  where
    mkServer addr   = Server host addr []
    getHostAddrInfo = getAddrInfo hints (Just host) (Just "ntp")

    hints = Just defaultHints { addrFamily     = AF_INET
                              , addrSocketType = Datagram }

updateServer :: Server -> NTP Server
updateServer svr = do
    transmit svr
    rs <- toList =<< receive
    return (insertRecords svr rs)
  where
    toList (Left _)  = return []
    toList (Right x) = return [x]

insertRecords :: Server -> [Record] -> Server
insertRecords svr xs = svr { svrRecords = records }
  where
    records = take 5 (xs ++ svrRecords svr)

transmit :: Server -> NTP ()
transmit Server{..} = do
    NTPData{..} <- ask
    liftIO $ do
        now <- getCurrentTime ntpClock
        let msg = (runPut . put) emptyNTPMsg { t3 = now }
        sendAllTo ntpSocket msg svrAddress

receive :: NTP (Either String Record)
receive = do
    NTPData{..} <- ask
    liftIO $ do
        mbs <- (handleIOErrors . timeout 1000000 . recv ntpSocket) 128
        t   <- getCurrentTime ntpClock
        return $ case mbs of
            Nothing -> Left "Timed out"
            Just bs -> record t <$> runGet get bs
  where
    handleIOErrors = handle (\(_ :: IOException) -> return Nothing)

    record t4 NTPMsg{..} = (meanUTC t1 t4, meanUTC t2 t3)

------------------------------------------------------------------------

meanInt :: Integral a => a -> a -> a
meanInt x y = ((y - x) `div` 2) + x

meanUTC :: UTCTime -> UTCTime -> UTCTime
meanUTC x y = ((y `diffUTCTime` x) / 2) `addUTCTime` x
