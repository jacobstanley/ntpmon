{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.NTP where

import           Control.Applicative ((<$>))
import           Control.Concurrent (forkIO)
import           Control.Concurrent.STM.TChan
import           Control.Exception (IOException, bracket, handle)
import           Control.Monad.IO.Class
import           Control.Monad.Loops (unfoldM)
import           Control.Monad.STM (atomically)
import           Control.Monad.State
import           Data.List (sortBy)
import           Data.Ord (comparing)
import           Data.Serialize (encode, decode)
import           Data.Time.Clock (UTCTime, NominalDiffTime, addUTCTime, diffUTCTime)
import qualified Data.Time.Clock as T
import qualified Data.Vector.Unboxed as V
import           Data.Word (Word64)
import           Network.Socket hiding (send, sendTo, recv, recvFrom)
import           Network.Socket.ByteString
import qualified Statistics.Sample as S

import           Data.NTP hiding (Server)
import           System.Counter
import           Text.PrefixSI

import           Debug.Trace

------------------------------------------------------------------------
-- NTP Monad

newtype NTP a = NTP { runNTP :: StateT NTPData IO a }
    deriving (Functor, Monad, MonadIO, MonadState NTPData)

data NTPData = NTPData {
      ntpSocket   :: Socket
    , ntpClock    :: Clock
    , ntpIncoming :: TChan AddrSample
    }

type AddrSample = (SockAddr, Sample)
type Logger = String -> IO ()

withNTP :: Logger -> NTP a -> IO a
withNTP logger =
    fmap fst . withSocketsDo . bracket create destroy . runStateT . runNTP
  where
    create = do
        ntpSocket   <- initSocket
        ntpClock    <- initCounterClock logger
        ntpIncoming <- newTChanIO

        -- receive packet loop
        let getClock = getCurrentIndex ntpClock
        forkIO $ forever $ do
            pkt <- receive ntpSocket getClock
            either logger (atomically . writeTChan ntpIncoming) pkt

        return NTPData{..}

    destroy NTPData{..} = sClose ntpSocket

    initSocket = do
        sock <- socket AF_INET Datagram defaultProtocol
        bindSocket sock (SockAddrInet 0 0)
        return sock

------------------------------------------------------------------------
-- Clock

type ClockIndex = Word64
type ClockDiff  = Word64
type ClockDiffD = Double
type Seconds    = Double

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

-- | Gets a clock difference in seconds
fromDiff :: Clock -> ClockDiff -> Seconds
fromDiff Clock{..} d = fromIntegral d / clockFrequency


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

setFrequency :: Double -> Clock -> Clock
setFrequency freq c = c { clockFrequency = freq }

------------------------------------------------------------------------
-- Server

data Server = Server {
      svrHostName     :: HostName
    , svrAddress      :: SockAddr
    , svrRawSamples   :: [Sample]
    , svrMinRoundtrip :: ClockDiff
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
    mkServer addr = Server host addr [] maxBound

    getHostAddrInfo = getAddrInfo hints (Just host) (Just "ntp")

    hints = Just defaultHints { addrFamily     = AF_INET
                              , addrSocketType = Datagram }

transmit :: Server -> NTP ()
transmit Server{..} = do
    NTPData{..} <- get
    liftIO $ do
        now <- getCurrentIndex ntpClock
        let msg = (encode . requestMsg . toTimestamp) now
        sendAllTo ntpSocket msg svrAddress

receive :: Socket -> IO ClockIndex -> IO (Either String AddrSample)
receive sock getClock = handleIOErrors $ do
    (bs, addr) <- recvFrom sock 128
    t          <- getClock
    return $ case decode bs of
        Left err  -> Left err
        Right pkt -> Right (addr, mkSample t pkt)
  where
    handleIOErrors = handle (\(e :: IOException) -> return (Left (show e)))

    mkSample t4 Packet{..} = Sample
        { rawT1 = fromTimestamp ntpT1
        , rawT2 = fromTimestamp ntpT2
        , rawT3 = fromTimestamp ntpT3
        , rawT4 = t4 }

updateServers :: [Server] -> NTP [Server]
updateServers svrs = do
    NTPData{..} <- get
    liftIO $ do
        pkts <- unfoldM (tryReadChan ntpIncoming)
        trace (unwords $ map (show . fst) pkts) $
            return (map (update pkts) svrs)
  where
    update :: [AddrSample] -> Server -> Server
    update = fconcat . map go
      where
        go (addr, s) svr
          | addr == svrAddress svr = updateServer svr s
          | otherwise              = svr

    fconcat :: [a -> a] -> a -> a
    fconcat = foldl (.) id

    tryReadChan chan = atomically $ do
        empty <- isEmptyTChan chan
        if empty then return Nothing
                 else Just <$> readTChan chan

updateServer :: Server -> Sample -> Server
updateServer svr s = svr
    { svrRawSamples = samples
    , svrMinRoundtrip = (minimum . map roundtrip) samples
    }
  where
    -- force the evaluation of the last sample in the list
    -- to avoid building up lots of unevaluated thunks
    samples = let xs = take maxSamples (s : svrRawSamples svr)
              in last xs `seq` xs


-- | Assumes the current frequency is the average of the last x seconds.
fudgeFrequencyEstimate :: Clock -> Sample -> Seconds -> Sample
fudgeFrequencyEstimate clock s nsecs = s
    { rawT1 = rawT1 s - round (nsecs * clockFrequency clock)
    , rawT2 = rawT2 s `add` realToFrac (-nsecs) }


maxSamples :: Int
maxSamples = max phaseSamples freqSamples

freqSamples :: Int
freqSamples = 1000 * samplesPerSecond

phaseSamples :: Int
phaseSamples = 10 * samplesPerSecond

samplesPerSecond :: Int
samplesPerSecond = 1

adjustClock :: Server -> Clock -> Maybe (Clock, Seconds)
adjustClock svr@Server{..} clock =
    traceShow phase $
    traceShow freq $
    if length svrRawSamples > 5
    then Just (adjust clock, phase)
    else Nothing
  where
    adjust = adjustOffset (realToFrac phase)
           . adjustFrequency freq
           . adjustOrigin earliestTime

    earliestSample = last svrRawSamples
    earliestTime   = rawT1 earliestSample

    phase     = S.meanWeighted (V.take phaseSamples weightedOffsets)
    (_, freq) = linearRegression (V.take freqSamples times)
                                 (V.take freqSamples weightedOffsets)

    weightedOffsets = V.zip offsets weights
    times   = doubleVector (fromDiff clock . (\x -> rawT4 x - earliestTime)) svrRawSamples
    offsets = doubleVector (offset clock) svrRawSamples
    weights = doubleVector (quality clock svr) svrRawSamples

    doubleVector :: (a -> Double) -> [a] -> V.Vector Double
    doubleVector f = V.fromList . map f

-- | Simple linear regression between 2 samples.
--   Takes two vectors Y={yi} and X={xi} and returns
--   (alpha, beta, r*r) such that Y = alpha + beta*X
linearRegression :: S.Sample -> S.WeightedSample -> (Double, Double)
linearRegression xs ys = (alpha, beta)
  where
    !c     = V.sum (V.zipWith (*) (V.map (subtract mx) xs) (V.map (subtract my . fst) ys)) / (n-1)
    !r     = c / (sx * sy)
    !mx    = S.mean xs
    !my    = S.meanWeighted ys
    !sx    = S.stdDev xs
    !sy    = sqrt (S.varianceWeighted ys)
    !n     = fromIntegral (V.length xs)
    !beta  = r * sy / sx
    !alpha = my - beta * mx
{-# INLINE linearRegression #-}

------------------------------------------------------------------------
-- Samples

data Sample = Sample {
      rawT1 :: ClockIndex
    , rawT2 :: UTCTime
    , rawT3 :: UTCTime
    , rawT4 :: ClockIndex
    } deriving (Show, Eq)

-- TODO Change to use uncorrected clock instead of the absolute clock
offset :: Clock -> Sample -> Seconds
offset clock Sample{..} = realToFrac (remote `sub` local)
  where
    local  = clockTime clock (rawT1 + (rawT4 - rawT1) `div` 2)
    remote = rawT2 `add` ((rawT3 `sub` rawT2) / 2)

roundtrip :: Sample -> ClockDiff
roundtrip Sample{..} = rawT4 - rawT1

quality :: Clock -> Server -> Sample -> Double
quality clock svr s = exp (-x*x)
  where
    x = sampleError / baseError

    sampleError = currentError clock svr s
    baseError   = 4 * estimatedHostDelay

initialError :: Clock -> Server -> Sample -> Seconds
initialError clock Server{..} s = fromDiff clock (roundtrip s - svrMinRoundtrip)

-- TODO could be more accurate if we calculated estimated frequency error
currentError :: Clock -> Server -> Sample -> Seconds
currentError clock svr s = initial + drift * age
  where
    initial = initialError clock svr s

    age = fromDiff clock (currentTime - sampleTime)
    currentTime = rawT4 (head (svrRawSamples svr))
    sampleTime  = rawT4 s

    drift = 1 / 10000000 -- 0.1 ppm

withinError :: Clock -> Server -> Sample -> Bool
withinError clock svr = (< allowedError) . go . initialError clock svr
  where
    allowedError = 20 * estimatedHostDelay
    go x = trace ("Min: " ++ show (svrMinRoundtrip svr) ++ "  Error: " ++ show x ++ "s  (Allowed: " ++ show allowedError ++ "s)") x

estimatedHostDelay :: Double
estimatedHostDelay = 15 / 1000 / 1000 -- 15 usec


data OldSample = OldSample {
      t1 :: UTCTime
    , t2 :: UTCTime
    , t3 :: UTCTime
    , t4 :: UTCTime
    } deriving (Show, Eq)

oldReify :: Clock -> Sample -> OldSample
oldReify clock Sample{..} = OldSample{..}
  where
    t1 = clockTime clock rawT1
    t2 = rawT2
    t3 = rawT3
    t4 = clockTime clock rawT4

oldOffset :: OldSample -> NominalDiffTime
oldOffset OldSample{..} = ((t2 `sub` t1) + (t3 `sub` t4)) / 2

oldDelay :: OldSample -> NominalDiffTime
oldDelay OldSample{..} =
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
clockFilter :: [OldSample] -> Maybe OldSample
clockFilter = go . take window
  where
    -- number of samples to consider in the sliding window
    window = 8

    go xs = if length xs == window
            then (Just . head . sortBy (comparing oldDelay)) xs
            else Nothing

-- | The difference between two absolute times.
sub :: UTCTime -> UTCTime -> NominalDiffTime
sub = diffUTCTime

-- | Adds an offset to an absolute time.
add :: UTCTime -> NominalDiffTime -> UTCTime
add = flip addUTCTime
