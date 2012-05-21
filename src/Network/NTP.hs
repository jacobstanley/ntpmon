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
import           Data.Serialize (encode, decode)
import           Data.Time.Clock (UTCTime, NominalDiffTime, addUTCTime, diffUTCTime)
import qualified Data.Time.Clock as T
import qualified Data.Vector.Unboxed as V
import           Data.Word (Word64)
import           Network.Socket hiding (send, sendTo, recv, recvFrom)
import           Network.Socket.ByteString
import qualified Statistics.Function as S
import qualified Statistics.Sample as S

import           Data.NTP hiding (Server)
import           System.Counter
import           Text.PrefixSI

--import           Debug.Trace
--import           Text.Printf (printf)

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

-- | The difference between two absolute times.
sub :: UTCTime -> UTCTime -> NominalDiffTime
sub = diffUTCTime

-- | Adds an offset to an absolute time.
add :: UTCTime -> NominalDiffTime -> UTCTime
add = flip addUTCTime

------------------------------------------------------------------------
-- Server

data Server = Server {
      svrHostName     :: HostName
    , svrAddress      :: SockAddr
    , svrRawSamples   :: [Sample]
    , svrMinRoundtrip :: ClockDiff
    , svrBaseError    :: ClockDiff
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
    mkServer addr = Server host addr [] 0 0

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
        { t1 = fromTimestamp ntpT1
        , t2 = fromTimestamp ntpT2
        , t3 = fromTimestamp ntpT3
        , t4 = t4 }

updateServers :: [Server] -> NTP [Server]
updateServers svrs = do
    NTPData{..} <- get
    liftIO $ do
        pkts <- unfoldM (tryReadChan ntpIncoming)
        --trace "" $
        --trace (unwords $ map (show . fst) pkts) $
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
updateServer svr s =
    --trace (printf "%-15s%8.0f%8.0f" (svrHostName svr) minRoundtrip stdDevRoundtrip) $
    svr
    { svrRawSamples   = samples
    , svrMinRoundtrip = round minRoundtrip
    , svrBaseError    = round (3 * stdDevRoundtrip)
    }
  where
    minRoundtrip    = V.head bestRoundtrips
    stdDevRoundtrip = S.stdDev bestRoundtrips

    -- Trim roundtrips so that outliers due to network congestion are
    -- not included in our quality weights.
    roundtrips     = vector (fromIntegral . roundtrip) samples
    bestRoundtrips = (V.take half . S.partialSort half) roundtrips
    half           = (V.length roundtrips + 1) `div` 2

    -- force the evaluation of the last sample in the list
    -- to avoid building up lots of unevaluated thunks
    samples = let xs = take maxSamples (s : svrRawSamples svr)
              in last xs `seq` xs

maxSamples :: Int
maxSamples = max phaseSamples freqSamples

freqSamples :: Int
freqSamples = round (1000 * samplesPerSecond)

phaseSamples :: Int
phaseSamples = round (50 * samplesPerSecond)

samplesPerSecond :: Double
samplesPerSecond = 0.5

adjustClock :: Server -> Clock -> Maybe (Clock, Seconds)
adjustClock svr@Server{..} clock =
    -- traceShow phase $
    -- traceShow freq $
    -- traceShow (V.take 5 weights) $
    if nsamples < 5 || isNaN freq || isNaN phase
    then Nothing
    else Just (adjust clock, phase)
  where
    adjust = adjustOffset (realToFrac phase)
           . adjustFrequency freq
           . adjustOrigin earliestTime

    nsamples       = length svrRawSamples
    earliestSample = last svrRawSamples
    earliestTime   = t1 earliestSample

    -- phase lock trades offset for jitter early on when
    -- the frequency lock hasn't settled yet
    phaseN = min phaseSamples ((nsamples + 4) `div` 5)
    phase  = S.meanWeighted (V.take phaseN weightedOffsets)

    (_, freq) = linearRegression (V.take freqSamples times)
                                 (V.take freqSamples weightedOffsets)

    weightedOffsets = V.zip offsets weights
    times   = vector (fromDiff clock . (\x -> t4 x - earliestTime)) svrRawSamples
    offsets = vector (offset clock) svrRawSamples
    weights = vector (quality clock svr) svrRawSamples

------------------------------------------------------------------------
-- Samples

data Sample = Sample {
      t1 :: ClockIndex
    , t2 :: UTCTime
    , t3 :: UTCTime
    , t4 :: ClockIndex
    } deriving (Show, Eq)

offset :: Clock -> Sample -> Seconds
offset clock sample = realToFrac (remoteTime sample `sub` localTime clock sample)

localTime :: Clock -> Sample -> UTCTime
localTime clock Sample{..} = clockTime clock (t1 + (t4 - t1) `div` 2)

remoteTime :: Sample -> UTCTime
remoteTime Sample{..} = t2 `add` ((t3 `sub` t2) / 2)

roundtrip :: Sample -> ClockDiff
roundtrip Sample{..} = t4 - t1

quality :: Clock -> Server -> Sample -> Double
quality clock svr s = exp (-x*x)
  where
    x = sampleError / baseError

    sampleError = currentError clock svr s
    baseError   = fromDiff clock (svrBaseError svr)

initialError :: Clock -> Server -> Sample -> Seconds
initialError clock Server{..} s = fromDiff clock (roundtrip s - svrMinRoundtrip)

-- TODO could be more accurate if we calculated estimated frequency error
currentError :: Clock -> Server -> Sample -> Seconds
currentError clock svr s = initial + drift * age
  where
    initial = initialError clock svr s

    age = fromDiff clock (currentTime - sampleTime)
    currentTime = t4 (head (svrRawSamples svr))
    sampleTime  = t4 s

    drift = 1 / 10000000 -- 0.1 ppm

------------------------------------------------------------------------
-- Vector/Statistics Utils

-- | Maps a functions over a list and turns it in to a vector.
vector :: (a -> Double) -> [a] -> V.Vector Double
vector f = V.fromList . map f

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

