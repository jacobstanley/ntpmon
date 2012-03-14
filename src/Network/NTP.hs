{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.NTP where

import           Control.Applicative ((<$>))
import           Control.Exception (IOException, bracket, handle)
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.List (nub, sortBy, tails)
import           Data.Ord (comparing)
import           Data.Maybe (mapMaybe)
import           Data.Serialize (encode, decode)
import           Data.Time.Clock (UTCTime, NominalDiffTime, addUTCTime, diffUTCTime)
import qualified Data.Time.Clock as T
import qualified Data.Vector.Unboxed as V
import           Data.Word (Word64)
import           Network.Socket hiding (send, sendTo, recv, recvFrom)
import           Network.Socket.ByteString
import           Statistics.LinearRegression (linearRegression)

import           Data.NTP hiding (Server)
import           System.Counter
import           Text.PrefixSI

--import           Statistics.Sample (mean)
--import           Debug.Trace

------------------------------------------------------------------------
-- NTP Monad

newtype NTP a = NTP { runNTP :: StateT NTPData IO a }
    deriving (Functor, Monad, MonadIO, MonadState NTPData)

data NTPData = NTPData {
      ntpSocket :: Socket
    , ntpClock  :: Clock
    }

type Logger = String -> IO ()

withNTP :: Logger -> NTP a -> IO a
withNTP logger =
    fmap fst . withSocketsDo . bracket create destroy . runStateT . runNTP
  where
    create = do
        ntpSocket <- socket AF_INET Datagram defaultProtocol
        -- set timeout to 1000ms, this is the minimum
        setSocketOption ntpSocket RecvTimeOut 1000
        ntpClock  <- initCounterClock logger
        return NTPData{..}

    destroy NTPData{..} = sClose ntpSocket

------------------------------------------------------------------------
-- Clock

type ClockIndex = Word64

data Clock = Clock {
      clockTime0      :: UTCTime
    , clockIndex0     :: ClockIndex
    , clockFrequency  :: Double
    , clockPrecision  :: Word64
    , getCurrentIndex :: IO ClockIndex
    }

-- | Initializes a 'Clock' based on the high precision counter.
initCounterClock :: Logger -> IO Clock
initCounterClock logger = do
    logger "Analyzing high precision counter..."

    counter <- analyzeCounter

    let clockFrequency = fromIntegral (cntFrequency counter)
        clockPrecision = cntPrecision counter
        freq = clockFrequency
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
clockTime Clock{..} index = clockTime0 `add` off
  where
    off  = realToFrac (diff / freq)
    diff = fromIntegral (index - clockIndex0)
    freq = clockFrequency

-- | Gets the index at the given time.
clockIndex :: Clock -> UTCTime -> ClockIndex
clockIndex Clock{..} time = clockIndex0 + round (diff * freq)
  where
    diff = realToFrac (time `sub` clockTime0)
    freq = clockFrequency


adjustFrequency :: Double -- ^ the drift rate in seconds/second
                -> Clock -> Clock
adjustFrequency adj c = c
    { clockFrequency = (1 - adj) * clockFrequency c }

adjustOffset :: NominalDiffTime -- ^ the offset in seconds
             -> Clock -> Clock
adjustOffset adj c = c
    { clockTime0 = clockTime0 c `add` adj }

adjustOrigin :: ClockIndex -- ^ the new clockIndex0
             -> Clock -> Clock
adjustOrigin newIndex0 c = c
    { clockTime0  = clockTime c newIndex0
    , clockIndex0 = newIndex0 }

------------------------------------------------------------------------
-- Server

data Server = Server {
      svrHostName   :: HostName
    , svrAddress    :: SockAddr
    , svrRawSamples :: [RawSample]
    } deriving (Show)

svrName :: Server -> String
svrName svr | host /= addr = host ++ " (" ++ addr ++ ")"
            | otherwise    = host
  where
    host = svrHostName svr
    addr = (takeWhile (/= ':') . show . svrAddress) svr

-- | Resolves a list of IP addresses registered for the specified
-- hostname and creates 'Server' instances for each of them.
resolveServers :: HostName -> IO [Server]
resolveServers host = map (mkServer . addrAddress) <$> getHostAddrInfo
  where
    mkServer addr   = Server host addr []
    getHostAddrInfo = getAddrInfo hints (Just host) (Just "ntp")

    hints = Just defaultHints { addrFamily     = AF_INET
                              , addrSocketType = Datagram }

updateServer :: Server -> NTP Server
updateServer svr = do
    transmit svr
    either (const svr) withSample <$> receive
  where
    withSample s =
        let samples = take maxSamples (s : svrRawSamples svr)
        -- Force the evaluation of the last sample in the list
        -- to avoid building up lots of unevaluated thunks.
        in (last samples) `seq` svr { svrRawSamples = samples }

maxSamples :: Int
maxSamples = 4 * 20

adjustClock :: Server -> Clock -> Maybe Clock
adjustClock Server{..} clock =
    --traceShow (length samples) $
    --traceShow (mean offsets) $
    --traceShow (mean times) $
    --traceShow (clockTime0 clock') $
    --traceShow (clockFrequency clock') $
    --traceShow (off*1000000,freq*1000000) $
    if length svrRawSamples == maxSamples
    then Just (adjust clock)
    else Nothing
  where
    adjust = adjustOffset (realToFrac off)
           . adjustFrequency freq
           . adjustOrigin earliestRawTime

    earliestRawTime = rawT1 (last svrRawSamples)

    samples      = extractBest svrRawSamples
    earliestTime = t1 (last samples)
    times        = doubleVector (realToFrac . (`sub` earliestTime) . t4) samples
    offsets      = doubleVector (realToFrac . offset) samples
    (off,freq)   = linearRegression times offsets

    extractBest = nub . mapMaybe clockFilter . tails . map (reify clock)

    doubleVector :: (a -> Double) -> [a] -> V.Vector Double
    doubleVector f = V.fromList . map f


transmit :: Server -> NTP ()
transmit Server{..} = do
    NTPData{..} <- get
    liftIO $ do
        now <- getCurrentIndex ntpClock
        let msg = (encode . requestMsg . toTimestamp) now
        sendAllTo ntpSocket msg svrAddress

receive :: NTP (Either String RawSample)
receive = do
    NTPData{..} <- get
    liftIO $ do
        mbs <- (handleIOErrors . fmap Just . recvFrom ntpSocket) 128
        t   <- getCurrentIndex ntpClock
        return $ case mbs of
            Nothing          -> Left "Timed out"
            Just (bs, _addr) -> mkSample t <$> decode bs
  where
    handleIOErrors = handle (\(_ :: IOException) -> return Nothing)

    mkSample t4 Packet{..} = RawSample
        { rawT1 = fromTimestamp ntpT1
        , rawT2 = fromTimestamp ntpT2
        , rawT3 = fromTimestamp ntpT3
        , rawT4 = t4 }

------------------------------------------------------------------------
-- Samples

data RawSample = RawSample {
      rawT1 :: ClockIndex
    , rawT2 :: UTCTime
    , rawT3 :: UTCTime
    , rawT4 :: ClockIndex
    } deriving (Show, Eq)

data Sample = Sample {
      t1 :: UTCTime
    , t2 :: UTCTime
    , t3 :: UTCTime
    , t4 :: UTCTime
    } deriving (Show, Eq)

reify :: Clock -> RawSample -> Sample
reify clock RawSample{..} = Sample{..}
  where
    t1 = clockTime clock rawT1
    t2 = rawT2
    t3 = rawT3
    t4 = clockTime clock rawT4

offset :: Sample -> NominalDiffTime
offset Sample{..} = ((t2 `sub` t1) + (t3 `sub` t4)) / 2

delay :: Sample -> NominalDiffTime
delay Sample{..} =
    -- If the server is very close to us, or even on the same computer,
    -- and its clock is not very precise (e.g. Win 7 is ~1ms) then the
    -- total roundtrip time can be less than the processing time. In
    -- this case just ignore the processing time.
    if roundtripTime < processingTime
    then roundtripTime
    else roundtripTime - processingTime
  where
    roundtripTime  = (t4 `sub` t1)
    processingTime = (t3 `sub` t2)

-- | The NTP clock filter. Select the sample (from the last 8) with the
-- lowest delay.
clockFilter :: [Sample] -> Maybe Sample
clockFilter = go . take window
  where
    -- number of samples to consider in the sliding window
    window = 4

    go xs = if length xs == window
            then (Just . head . sortBy (comparing delay)) xs
            else Nothing

-- | The difference between two absolute times.
sub :: UTCTime -> UTCTime -> NominalDiffTime
sub = diffUTCTime

-- | Adds an offset to an absolute time.
add :: UTCTime -> NominalDiffTime -> UTCTime
add = flip addUTCTime
