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
import           System.Timeout (timeout)

import           Network.NTP.Types hiding (Server)
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
clockTime Clock{..} index = clockTime0 `add` (diff / freq)
  where
    diff = fromIntegral (index - clockIndex0)
    freq = fromIntegral clockFrequency

-- | Gets the index at the given time.
clockIndex :: Clock -> UTCTime -> ClockIndex
clockIndex Clock{..} time = clockIndex0 + round (diff * freq)
  where
    diff = time `sub` clockTime0
    freq = fromIntegral clockFrequency


adjustFrequency :: Double -- ^ the drift rate in seconds/second
                -> Clock -> Clock
adjustFrequency adj c = c
    { clockFrequency = round ((1 - adj) * fromIntegral (clockFrequency c)) }

adjustOffset :: Double -- ^ the offset in seconds
             -> Clock -> Clock
adjustOffset adj c = c
    { clockTime0 = clockTime0 c `add` realToFrac adj }

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
resolveServers :: HostName -> NTP [Server]
resolveServers host = liftIO $
    map (mkServer . addrAddress) <$> getHostAddrInfo
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
    withSample s = svr { svrRawSamples = take maxSamples (s : svrRawSamples svr) }

maxSamples :: Int
maxSamples = 200

adjustClock :: Server -> Clock -> Maybe Clock
adjustClock Server{..} clock =
    --traceShow (length samples) $
    --traceShow (mean offsets) $
    --traceShow (mean times) $
    --traceShow (clockTime0 clock') $
    --traceShow (clockFrequency clock') $
    --traceShow (off*1000000,freq*1000000) $
    if length samples > 2
    then Just clock'
    else Nothing
  where
    clock' = (adjustOffset off . adjustFrequency freq) clock

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
        now <- getCurrentTime ntpClock
        let msg = encode emptyNTPMsg { ntpT3 = now }
        sendAllTo ntpSocket msg svrAddress

receive :: NTP (Either String RawSample)
receive = do
    NTPData{..} <- get
    liftIO $ do
        mbs <- (handleIOErrors . timeout 1000000 . recv ntpSocket) 128
        t   <- getCurrentIndex ntpClock
        return $ case mbs of
            Nothing -> Left "Timed out"
            Just bs -> mkSample ntpClock t <$> decode bs
  where
    handleIOErrors = handle (\(_ :: IOException) -> return Nothing)

    mkSample clock t4 NTPMsg{..} = RawSample
        { rawT1 = clockIndex clock ntpT1
        , rawT2 = ntpT2
        , rawT3 = ntpT3
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
    window = 8

    go xs = if length xs == window
            then (Just . head . sortBy (comparing delay)) xs
            else Nothing

-- | The difference between two absolute times.
sub :: UTCTime -> UTCTime -> NominalDiffTime
sub = diffUTCTime

-- | Adds an offset to an absolute time.
add :: UTCTime -> NominalDiffTime -> UTCTime
add = flip addUTCTime
