{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

--module Main (main) where
module Main where

import           Control.Applicative ((<$>))
import           Control.Concurrent (threadDelay)
import           Control.Exception (IOException, bracket, handle)
import           Control.Monad (when, replicateM, forever)
import           Data.List (intercalate)
import qualified Data.ByteString as B
import           Data.Serialize
import qualified Data.Vector.Unboxed as V
import           Data.Time.Clock
import           Data.Time.Format
import           Data.Word (Word64)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import           Network.Socket hiding (send, sendTo, recv, recvFrom)
import           Network.Socket.ByteString
import           System.Environment (getArgs)
import           System.Locale (defaultTimeLocale)
import           System.IO
import           Statistics.Sample
import           Statistics.Distribution.Normal
import           System.Timeout (timeout)
import           Text.Printf (printf)

import           Data.NTP hiding (Server)
import           System.Counter (readCounter, analyzeFrequency)
--import           System.CPUTime (getCPUTime)

------------------------------------------------------------------------

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering

    hPutStr stderr "Analyzing high performance counter..."
    freq <- analyzeFrequency
    hPutStrLn stderr "done"
    hPutStrLn stderr ("Frequency = " ++ showFreq (fromIntegral freq))

    --putStrLn $ unwords $ ["drift_us", "last", "mean", "stddev%", "range%", "kurtosis", "skewness"]
    --xs <- reverse <$> replicateM 5 getSample
    --loop xs
  where
    loop xs = do
        x <- getSample

        let xs' = x : take 200 xs
            ds = V.fromList (map2 dcdt xs')
            d  = dcdt x (head xs)
            drift = 1000000 * (1.0 - d / mean ds)

            _n  = normalFromSample ds
            sd_pct = 100.0 * (stdDev ds / mean ds)
            rng_pct = 100.0 * (range ds / mean ds)

        putStrLn $ unwords $ map showD
            [drift, d, mean ds, sd_pct, rng_pct, kurtosis ds, skewness ds]
        loop xs'

    getSample :: IO (Counter, UTCTime)
    getSample = do
        threadDelay 200000
        t <- getCurrentTime
        c <- readCounter
        return (c, t)

    dcdt :: (Counter, UTCTime) -> (Counter, UTCTime) -> Double
    dcdt (c1, t1) (c0, t0) = dc / dt :: Double
      where
        dc = fromIntegral (c1 - c0)
        dt = realToFrac (t1 `diffUTCTime` t0)

    showD :: Double -> String
    showD = printf "%.8f"

    map2 :: (a -> a -> b) -> [a] -> [b]
    map2 _ []  = []
    map2 _ [_] = []
    map2 f (x0:x1:xs) = f x0 x1 : map2 f (x1:xs)

showFreq :: Double -> String
showFreq hz | hz < kilo = format "" 1
            | hz < mega = format "K" kilo
            | hz < giga = format "M" mega
            | otherwise = format "G" giga
  where
    format unit size = printf "%.2f %sHz" (hz / size) unit
    kilo = 1000
    mega = 1000 * kilo
    giga = 1000 * mega

main1 :: IO ()
main1 = withSocketsDo $ do
    hosts <- getArgs
    if length hosts < 2
       then putStr usage
       else do
         (reference:servers) <- concat <$> mapM getServers hosts
         monitor reference servers

usage :: String
usage = "ntp-monitor 0.1\n\
\Usage: ntp-monitor REFERENCE SERVER [SERVER]..\
\\n\
\\n  REFERENCE  The NTP server which the other servers will be measured\
\\n             against.\
\\n\
\\n  SERVER     An NTP server to monitor.\
\\n\
\\nNTP servers can be specified using either their hostname or IP address.\
\\n"

monitor :: Server -> [Server] -> IO ()
monitor ref ss = do
    (putStrLn . intercalate "," . map fst) headers
    (putStrLn . intercalate "," . map snd) headers

    bracket udpSocket sClose (monitorLoop ref ss)
  where
    headers = [ (svrName ref ++ " - Unix Time", "Seconds Since 1970")
              , (svrName ref ++ " - UTC Time", "UTC Time") ]
           ++ map (\s -> (svrName s ++ " - Offset", "Milliseconds")) ss
           ++ [ ("Counter Frequency", "MHz") ]

monitorLoop :: Server -> [Server] -> Socket -> IO ()
monitorLoop ref ss sock = do
    ref' <- updateServer sock ref
    ss'  <- mapM (updateServer sock) ss

    let refRecords = svrRecords ref'

    when (length refRecords >= 3) $ do
        let (c0, utc0)   = refRecords !! 1 -- 2nd latest record
            (c1, utc1)   = refRecords !! 0 -- latest record
            deltaCounter = (fromIntegral (c1 - c0) / (utc1 `diffUTCTime` utc0))
                         / 1000000.0
                      -- / 1000000000.0
                      -- / (3.0666666666667 * 1000000000.0) -- 3.067 GHz
                      -- / (2.995 * 1000000.0)    -- 2.995 MHz

            offsets   = map (showOffset . calcOffset ref) ss'

            utcTime   = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S.%q" utc1
            unixTime  = (init . show) (utcTimeToPOSIXSeconds utc1)
            clockRate = (init . show) deltaCounter

            fields    = [unixTime, utcTime] ++ offsets ++ [clockRate]

        putStrLn (intercalate "," fields)

    threadDelay 1000000
    monitorLoop ref' ss' sock
  where
    showOffset = maybe "Unknown" showMilli

------------------------------------------------------------------------
-- Types

data Server = Server {
      svrHostName :: HostName
    , svrAddress  :: SockAddr
    , svrRecords  :: [Record]
    } deriving (Show)

type Counter = Word64

type Record = (Counter, UTCTime)

svrName :: Server -> String
svrName svr | host /= addr = addr ++ " (" ++ host ++ ")"
            | otherwise    = addr
  where
    host = svrHostName svr
    addr = (takeWhile (/= ':') . show . svrAddress) svr

------------------------------------------------------------------------

-- | Resolves a list of IP addresses registered for the specified
-- hostname and creates 'Server' instances for each of them.
getServers :: HostName -> IO [Server]
getServers host =
    map (mkServer . addrAddress) <$> getHostAddrInfo
  where
    mkServer addr   = Server host addr []
    getHostAddrInfo = getAddrInfo hints (Just host) (Just "ntp")

    hints = Just defaultHints { addrFamily     = AF_INET
                              , addrSocketType = Datagram }

updateServer :: Socket -> Server -> IO Server
updateServer sock svr = do
    t1 <- ntpSend sock (svrAddress svr)
    rs <- toList =<< ntpRecv sock t1
    return (insertRecords svr rs)
  where
    toList (Left _)  = return []
    toList (Right x) = return [x]

insertRecords :: Server -> [Record] -> Server
insertRecords svr xs = svr { svrRecords = records }
  where
    records = take 5 (xs ++ svrRecords svr)

calcOffset :: Server -> Server -> Maybe NominalDiffTime
calcOffset x y = (`diffUTCTime` t) <$> (timeAt c y)
  where
    (c, t) = lastRecord x

lastRecord :: Server -> Record
lastRecord = head . svrRecords

timeAt :: Counter -> Server -> Maybe UTCTime
timeAt c Server{..} =
    if null r2 || null r1
    then Nothing
    else Just (interp c (head r1) (last r2))
  where
    (r2, r1) = span ((> c) . fst) svrRecords

interp :: Counter -> Record -> Record -> UTCTime
interp c (c0, t0) (c1, t1) = lerpUTC alpha t0 t1
  where
    alpha = fromIntegral (c - c0) / fromIntegral (c1 - c0)

lerpUTC :: NominalDiffTime -> UTCTime -> UTCTime -> UTCTime
lerpUTC alpha t0 t1 = ((t1 `diffUTCTime` t0) * alpha) `addUTCTime` t0

------------------------------------------------------------------------

udpSocket :: IO Socket
udpSocket = socket AF_INET Datagram defaultProtocol

ntpSend :: Socket -> SockAddr -> IO Counter
ntpSend sock addr = do
    now <- getCurrentTime
    let msg = emptyNTPMsg { ntpTransmitTime = now }
        bs  = runPut (put msg)
    count <- (B.length bs) `seq` readCounter
    sendAllTo sock bs addr
    return count

ntpRecv :: Socket -> Counter -> IO (Either String Record)
ntpRecv sock t1 = do
    mbs <- (handleIOErrors . timeout 1000000 . recv sock) 128
    t4 <- readCounter
    return $ case mbs of
      Nothing -> Left "Timed out"
      Just bs -> record t4 <$> runGet get bs
  where
    handleIOErrors = handle (\(_ :: IOException) -> return Nothing)

    record t4 NTPMsg {..} =
        (meanInt t1 t4, meanUTC ntpReceiveTime ntpTransmitTime)

------------------------------------------------------------------------

meanInt :: Integral a => a -> a -> a
meanInt x y = ((y - x) `div` 2) + x

meanUTC :: UTCTime -> UTCTime -> UTCTime
meanUTC x y = ((y `diffUTCTime` x) / 2) `addUTCTime` x

showMilli :: NominalDiffTime -> String
showMilli t = printf "%.4f" ms
  where
    ms = (1000 :: Double) * (realToFrac t)
