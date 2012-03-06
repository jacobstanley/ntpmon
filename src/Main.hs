{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

--module Main (main) where
module Main where

import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (when)
import Data.List (intercalate)
import Data.Serialize
import Data.Time.Clock
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Word (Word64)
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import System.CPUTime.Rdtsc (rdtsc)
import System.IO
import System.Timeout (timeout)
import Text.Printf (printf)

import Data.NTP hiding (Server, getTime)

------------------------------------------------------------------------

hosts :: [HostName]
hosts =
    [ "localhost"
    , "time.uwa.edu.au"
    , "ntp.ii.net"
    , "203.171.85.237" -- PPS
    , "ntp.tourism.wa.gov.au"
    ]

main :: IO ()
main = withSocketsDo $ do
    hSetBuffering stdout LineBuffering
    servers@(x:xs) <- concat <$> mapM getServers hosts

    let headers = [showAddress x, "CPU"] ++ map showAddress xs
        units   = ["seconds since 1970", "clock cycles"]
               ++ replicate ((length servers) - 1) "ms"

    putStrLn (intercalate "," headers)
    putStrLn (intercalate "," units)
    bracket udpSocket sClose (loop servers)
  where
    loop servers sock = do
        servers'@(ref:others) <- mapM (update sock) servers

        let refRecords = svrRecords ref
        when (length refRecords >= 2) $ do
            let (i0, utc) = refRecords !! 0
                (i1, _)   = refRecords !! 1
                offsets   = map (showOffset . calcOffset ref) others
                unixTime  = (init . show) (utcTimeToPOSIXSeconds utc)
                fields    = [unixTime, show i0] ++ offsets

            putStrLn (intercalate "," fields)

        threadDelay 1000000
        loop servers' sock

    update sock svr = do
        svr' <- updateServer sock svr
        return svr'

    showAddress = (takeWhile (/= ':') . show) . svrAddress

    showOffset offset = (maybe "Unknown" showMilli offset)

------------------------------------------------------------------------
-- Types

data Server = Server {
      svrHostName :: HostName
    , svrAddress  :: SockAddr
    , svrRecords  :: [Record]
    } deriving (Show)

type Record = (Timestamp, UTCTime)

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
calcOffset x y = (`diffUTCTime` t) <$> (timeAt i y)
  where
    (i, t) = lastRecord x

lastRecord :: Server -> Record
lastRecord = head . svrRecords

timeAt :: Timestamp -> Server -> Maybe UTCTime
timeAt i Server{..} =
    if null r2 || null r1
    then Nothing
    else Just (interp i (head r1) (last r2))
  where
    (r2, r1) = span ((> i) . fst) svrRecords

interp :: Timestamp -> Record -> Record -> UTCTime
interp i (i0, t0) (i1, t1) = t
  where
    t = ((t1 `diffUTCTime` t0) * alpha) `addUTCTime` t0
    alpha = fromIntegral (i - i0) / fromIntegral (i1 - i0)

------------------------------------------------------------------------

udpSocket :: IO Socket
udpSocket = socket AF_INET Datagram defaultProtocol

ntpSend :: Socket -> SockAddr -> IO Timestamp
ntpSend sock addr = do
    now <- getCurrentTime
    timestamp <- getTimestamp
    let msg = emptyNTPMsg { ntpTransmitTime = now }
        bs  = runPut (put msg)
    sendAllTo sock bs addr
    return timestamp

ntpRecv :: Socket -> Timestamp -> IO (Either String Record)
ntpRecv sock t1 = do
    mbs <- timeout 1000000 (recv sock 128)
    t4 <- getTimestamp
    return $ case mbs of
      Nothing -> Left "Timed out"
      Just bs -> record t4 <$> runGet get bs
  where
    record t4 NTPMsg {..} =
        (mean t1 t4, meanUTC ntpReceiveTime ntpTransmitTime)

------------------------------------------------------------------------

mean :: Integral a => a -> a -> a
mean x y = ((y - x) `div` 2) + x

meanUTC :: UTCTime -> UTCTime -> UTCTime
meanUTC x y = ((y `diffUTCTime` x) / 2) `addUTCTime` x

showMilli :: NominalDiffTime -> String
showMilli t = printf "%.4f" ms
  where
    ms = (1000 :: Double) * (realToFrac t)

------------------------------------------------------------------------

type Timestamp = Word64

getTimestamp :: IO Timestamp
getTimestamp = rdtsc
