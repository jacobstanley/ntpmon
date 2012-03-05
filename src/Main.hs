{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

--module Main (main) where
module Main where

import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Data.List (intercalate)
import Data.Serialize
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock
import Data.Word (Word64)
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import System.CPUTime.Rdtsc (rdtsc)
import System.IO
import System.Posix.Clock (Clock (..), TimeSpec (..), getTime)
import System.Timeout (timeout)
import Text.Printf (printf)

import Data.NTP hiding (Server, getTime)

------------------------------------------------------------------------

hosts :: [HostName]
hosts =
    [ "localhost"
    , "time.uwa.edu.au"
    , "203.171.85.237" -- PPS
    , "ntp.ii.net"
    , "JakeAir.local"
    --, "au.pool.ntp.org"
    --, "1.au.pool.ntp.org"
    --, "2.au.pool.ntp.org"
    --, "3.au.pool.ntp.org"
    --, "fi.pool.ntp.org"
    ]

main :: IO ()
main = withSocketsDo $ do
    hSetBuffering stdout LineBuffering
    servers <- concat <$> mapM getServers hosts
    putStrLn (intercalate "," (map showAddress servers))
    bracket udpSocket sClose (loop servers)
  where
    loop servers sock = do
        servers' <- mapM (update sock) servers
        --putStrLn "NTP Server Offsets"
        putStrLn (intercalate "," (map showOffset (calcOffsets servers')))
        --putStrLn ""
        threadDelay 1000000
        loop servers' sock

    update sock svr = do
        svr' <- updateServer sock svr
        return svr'

    showAddress = (takeWhile (/= ':') . show) . svrAddress

    showOffset (_, offset) = (maybe "Unknown" showMilli offset)

    -- showOffset (Server {..}, offset) = printf "%-18s %-16s %s\n"
    --     svrHostName
    --     ((reverse . drop 4 . reverse . show) svrAddress)
    --     (maybe "Unknown Offset" showMilli offset)

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
    toList (Left err) = return [] --putStrLn ("Error: " ++ err) >> return []
    toList (Right x)  = return [x]

insertRecords :: Server -> [Record] -> Server
insertRecords svr xs = svr { svrRecords = records }
  where
    records = take 5 (xs ++ svrRecords svr)

calcOffsets :: [Server] -> [(Server, Maybe NominalDiffTime)]
calcOffsets []     = []
calcOffsets (x:xs) = (x, Just 0) : map go xs
  where
    (i, t) = (head . svrRecords) x
    go x'  = let t'     = timeAt i (svrRecords x')
                 offset = (`diffUTCTime` t) <$> t'
             in (x', offset)

timeAt :: Timestamp -> [Record] -> Maybe UTCTime
timeAt i rs = case span ((> i) . fst) rs of
    (r2:_, r1:_) -> Just (interp i r1 r2)
    _            -> Nothing

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
--showMilli t = printf "%+.4fms" ms
showMilli t = printf "%.4f" ms
  where
    ms = (1000 :: Double) * (realToFrac t)

------------------------------------------------------------------------

type Timestamp = Word64

getTimestamp :: IO Timestamp
getTimestamp = rdtsc

--getTimestamp :: IO Timestamp
--getTimestamp = do
--    TimeSpec {..} <- getTime Monotonic
--    let timestamp = (fromIntegral sec) * 1000000
--                  + ((fromIntegral nsec) `div` 1000)
--    return timestamp

--getTimestamp = do
--    now <- getCurrentTime
--    let diff = (now `diffUTCTime` origin)
--        timestamp = truncate (10000000 * diff)
--    return timestamp
--  where
--    origin = UTCTime (fromGregorian 2012 1 1) 0
