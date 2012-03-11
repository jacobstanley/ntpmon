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
import           Data.Time.Clock (UTCTime, NominalDiffTime, addUTCTime, diffUTCTime)
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
    }

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

-- | Gets the index at the given time.
clockIndex :: Clock -> UTCTime -> ClockIndex
clockIndex Clock{..} time = clockIndex0 + round (diff * freq)
  where
    diff = time `diffUTCTime` clockTime0
    freq = fromIntegral clockFrequency

------------------------------------------------------------------------
-- Server

data Server = Server {
      svrHostName :: HostName
    , svrAddress  :: SockAddr
    , svrSamples  :: [RawSample]
    } deriving (Show)

svrName :: Server -> String
svrName svr | host /= addr = host ++ " (" ++ addr ++ ")"
            | otherwise    = host
  where
    host = svrHostName svr
    addr = (takeWhile (/= ':') . show . svrAddress) svr

data RawSample = RawSample {
      rawT1 :: ClockIndex
    , rawT2 :: UTCTime
    , rawT3 :: UTCTime
    , rawT4 :: ClockIndex
    } deriving (Show)

data Sample = Sample {
      t1 :: UTCTime
    , t2 :: UTCTime
    , t3 :: UTCTime
    , t4 :: UTCTime
    } deriving (Show)

reify :: Clock -> RawSample -> Sample
reify clock RawSample{..} = Sample{..}
  where
    t1 = clockTime clock rawT1
    t2 = rawT2
    t3 = rawT3
    t4 = clockTime clock rawT4

offset :: Sample -> NominalDiffTime
offset Sample{..} = ((t2 `diffUTCTime` t1) + (t3 `diffUTCTime` t4)) / 2

delay :: Sample -> NominalDiffTime
delay Sample{..} = (t4 `diffUTCTime` t1) - (t3 `diffUTCTime` t2)

type Record = (ClockIndex, UTCTime)

record :: RawSample -> Record
record RawSample{..} = (meanInt rawT1 rawT4, meanUTC rawT2 rawT3)

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
    ms <- receive
    case ms of
        Left _  -> return svr
        Right s -> return (insertSample svr s)

insertSample :: Server -> RawSample -> Server
insertSample svr x = svr { svrSamples = samples }
  where
    samples = take 8 (x : svrSamples svr)

transmit :: Server -> NTP ()
transmit Server{..} = do
    NTPData{..} <- ask
    liftIO $ do
        now <- getCurrentTime ntpClock
        let msg = (runPut . put) emptyNTPMsg { ntpT3 = now }
        sendAllTo ntpSocket msg svrAddress

receive :: NTP (Either String RawSample)
receive = do
    NTPData{..} <- ask
    liftIO $ do
        mbs <- (handleIOErrors . timeout 1000000 . recv ntpSocket) 128
        t   <- getCurrentIndex ntpClock
        return $ case mbs of
            Nothing -> Left "Timed out"
            Just bs -> mkSample ntpClock t <$> runGet get bs
  where
    handleIOErrors = handle (\(_ :: IOException) -> return Nothing)

    mkSample clock t4 NTPMsg{..} = RawSample
        { rawT1 = clockIndex clock ntpT1
        , rawT2 = ntpT2
        , rawT3 = ntpT3
        , rawT4 = t4 }

------------------------------------------------------------------------

meanInt :: Integral a => a -> a -> a
meanInt x y = ((y - x) `div` 2) + x

meanUTC :: UTCTime -> UTCTime -> UTCTime
meanUTC x y = ((y `diffUTCTime` x) / 2) `addUTCTime` x
