{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Network.NTP where

import           Control.Applicative ((<$>))
import           Control.Concurrent (forkIO)
import           Control.Concurrent.STM.TChan
import           Control.Exception (IOException, bracket, handle)
import           Control.Monad.IO.Class
import           Control.Monad.Loops (unfoldM)
import           Control.Monad.STM (atomically)
import           Control.Monad.State
import qualified Data.ByteString as B
import           Data.Serialize (encode, decode)
import qualified Data.Time.Clock as T
import qualified Data.Vector.Generic as Generic
import qualified Data.Vector.Generic.Mutable as MGeneric
import qualified Data.Vector.Unboxed as U
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
    , ntpIncoming :: TChan AddrPacket
    }

data AddrPacket = AddrPacket ClockIndex SockAddr Packet
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
      clockTime0      :: Time
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
    clockTime0  <- fromUTCTime <$> T.getCurrentTime
    clockIndex0 <- getCurrentIndex

    return Clock{..}

-- | Gets the current time according to the 'Clock'.
getCurrentTime :: Clock -> IO Time
getCurrentTime clock = clockTime clock <$> getCurrentIndex clock

-- | Gets the time at the given index.
clockTime :: Clock -> ClockIndex -> Time
clockTime Clock{..} index = clockTime0 `add` fromSeconds off
  where
    off  = diff / freq
    -- TODO: Unsigned subtract breaks when index is earlier than clockIndex0.
    --       This is the case for servers which have not responded for a while
    --       as we continually move clockIndex0 forward using adjustOrigin.
    diff = fromIntegral (index - clockIndex0)
    freq = clockFrequency

-- | Gets the index at the given time.
clockIndex :: Clock -> Time -> ClockIndex
clockIndex Clock{..} time = clockIndex0 + round (diff * freq)
  where
    diff = toSeconds (time `sub` clockTime0)
    freq = clockFrequency

-- | Gets a clock difference in seconds
fromDiff :: Clock -> ClockDiff -> Seconds
fromDiff Clock{..} d = fromIntegral d / clockFrequency


adjustFrequency :: Double -- ^ the drift rate in seconds/second
                -> Clock -> Clock
adjustFrequency adj c = c
    { clockFrequency = (1 - adj) * clockFrequency c }

adjustOffset :: Seconds -- ^ the offset in seconds
             -> Clock -> Clock
adjustOffset adj c = c
    { clockTime0 = clockTime0 c `add` fromSeconds adj }

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
    , svrRawSamples   :: U.Vector Sample
    , svrMinRoundtrip :: ClockDiff
    , svrBaseError    :: ClockDiff
    , svrStratum      :: Int
    , svrReferenceId  :: B.ByteString
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
    mkServer addr = Server host addr U.empty 0 0 0 B.empty

    getHostAddrInfo = getAddrInfo hints (Just host) (Just "ntp")

    hints = Just defaultHints { addrFamily     = AF_INET
                              , addrSocketType = Datagram }

transmit :: Server -> NTP ()
transmit Server{..} = do
    NTPData{..} <- get
    liftIO $ do
        now <- getCurrentIndex ntpClock
        let msg = (encode . requestMsg . Time) now
        sendAllTo ntpSocket msg svrAddress

receive :: Socket -> IO ClockIndex -> IO (Either String AddrPacket)
receive sock getClock = handleIOErrors $ do
    (bs, addr) <- recvFrom sock 128
    t          <- getClock
    return $ case decode bs of
        Left err  -> Left err
        Right pkt -> Right (AddrPacket t addr pkt)
  where
    handleIOErrors = handle (\(e :: IOException) -> return (Left (show e)))

updateServers :: [Server] -> NTP [(Server, Bool)]
updateServers svrs = do
    NTPData{..} <- get
    liftIO $ do
        pkts <- unfoldM (tryReadChan ntpIncoming)
        return $ map (update pkts) (zip svrs (repeat False))
  where
    update :: [AddrPacket] -> (Server, Bool) -> (Server, Bool)
    update = fconcat . map go
      where
        go (AddrPacket t addr pkt) (svr, up)
          | addr == svrAddress svr = (updateServer svr t pkt, True)
          | otherwise              = (svr, up)

    fconcat :: [a -> a] -> a -> a
    fconcat = foldl (.) id

    tryReadChan chan = atomically $ do
        empty <- isEmptyTChan chan
        if empty then return Nothing
                 else Just <$> readTChan chan

updateServer :: Server -> ClockIndex -> Packet -> Server
updateServer svr t4 p@Packet{..} =
    svr
    { svrRawSamples   = samples
    , svrMinRoundtrip = round minRoundtrip
    , svrBaseError    = round (3 * stdDevRoundtrip)
    , svrStratum      = fromIntegral ntpStratum
    , svrReferenceId  = decodeReferenceId p
    }
  where
    newSample = (unTime ntpT1, ntpT2, ntpT3, t4)

    -- force the evaluation of the last sample in the list
    -- to avoid building up lots of unevaluated thunks
    samples = let xs = U.take maxSamples (newSample `U.cons` svrRawSamples svr)
              in U.last xs `seq` xs

    minRoundtrip    = U.head bestRoundtrips
    stdDevRoundtrip = S.stdDev bestRoundtrips

    -- Trim roundtrips so that outliers due to network congestion are
    -- not included in our quality weights.
    roundtrips     = U.map (fromIntegral . roundtrip) samples
    bestRoundtrips = (U.take half . S.partialSort half) roundtrips
    half           = (U.length roundtrips + 1) `div` 2


maxSamples :: Int
maxSamples = max phaseSamples freqSamples

freqSamples :: Int
freqSamples = round (1000 * samplesPerSecond)

phaseSamples :: Int
phaseSamples = round (50 * samplesPerSecond)

samplesPerSecond :: Double
samplesPerSecond = 0.5

adjustClock :: Server -> Clock -> (Clock, Seconds)
adjustClock svr@Server{..} clock = (adjust clock, phase)
  where
    adjust = if isNaN phase then id else adjustOffset phase
           . if isNaN freq  then id else adjustFrequency freq
           . adjustOrigin earliestTime

    earliestTime = time1 (U.last svrRawSamples)

    phase  = S.meanWeighted (U.take phaseSamples weightedOffsets)

    (_, freq) = linearRegression (U.take freqSamples times)
                                 (U.take freqSamples weightedOffsets)

    weightedOffsets = U.zip offsets weights
    times   = U.map (fromDiff clock . (\x -> time4 x - earliestTime)) svrRawSamples
    offsets = U.map (toSeconds . offset clock) svrRawSamples
    weights = U.map (quality clock svr) svrRawSamples

------------------------------------------------------------------------
-- Sample Type

type Sample = (ClockIndex, Time, Time, ClockIndex)

time1 :: Sample -> ClockIndex
time1 (x,_,_,_) = x

time2 :: Sample -> Time
time2 (_,x,_,_) = x

time3 :: Sample -> Time
time3 (_,_,x,_) = x

time4 :: Sample -> ClockIndex
time4 (_,_,_,x) = x

deriving instance MGeneric.MVector U.MVector Time
deriving instance Generic.Vector U.Vector Time
deriving instance U.Unbox Time

------------------------------------------------------------------------
-- Sample Functions

offset :: Clock -> Sample -> Duration
offset clock sample = remoteTime sample `sub` localTime clock sample

localTime :: Clock -> Sample -> Time
localTime clock (t1,_,_,t4) = clockTime clock (t1 + (t4 - t1) `div` 2)

remoteTime :: Sample -> Time
remoteTime (_,t2,t3,_) = mid t2 t3

networkDelay :: Clock -> Sample -> Duration
networkDelay clock sample = total - server
  where
    total  = fromSeconds (fromDiff clock (roundtrip sample))
    server = serverDelay sample

roundtrip :: Sample -> ClockDiff
roundtrip (t1,_,_,t4) = t4 - t1

serverDelay :: Sample -> Duration
serverDelay (_,t2,t3,_) = t3 `sub` t2

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
    currentTime = time4 (U.head (svrRawSamples svr))
    sampleTime  = time4 s

    drift = 1 / 10000000 -- 0.1 ppm

------------------------------------------------------------------------
-- Vector/Statistics Utils

-- | Maps a functions over a list and turns it in to a vector.
vector :: (a -> Double) -> [a] -> U.Vector Double
vector f = U.fromList . map f

-- | Simple linear regression between 2 samples.
--   Takes two vectors Y={yi} and X={xi} and returns
--   (alpha, beta, r*r) such that Y = alpha + beta*X
linearRegression :: S.Sample -> S.WeightedSample -> (Double, Double)
linearRegression xs ys = (alpha, beta)
  where
    !c     = U.sum (U.zipWith (*) (U.map (subtract mx) xs) (U.map (subtract my . fst) ys)) / (n-1)
    !r     = c / (sx * sy)
    !mx    = S.mean xs
    !my    = S.meanWeighted ys
    !sx    = S.stdDev xs
    !sy    = sqrt (S.varianceWeighted ys)
    !n     = fromIntegral (U.length xs)
    !beta  = r * sy / sx
    !alpha = my - beta * mx
{-# INLINE linearRegression #-}

