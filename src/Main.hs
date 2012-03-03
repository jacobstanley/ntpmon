{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

--module Main (main) where
module Main where

import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (forever)
import Data.List (intercalate)
import Data.Serialize
import Data.Time.Clock
import Data.Time.Format
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import System.CPUTime.Rdtsc (rdtsc)
import System.Locale (defaultTimeLocale)
import Text.Printf (printf)

import Data.NTP

------------------------------------------------------------------------

servers :: [HostName]
servers = --[ "localhost"
          --, "0.au.pool.ntp.org"
          --, "1.au.pool.ntp.org"
          --, "2.au.pool.ntp.org"
          --, "3.au.pool.ntp.org"
          [ "203.82.209.217"
          ]

resolveAddrs :: [HostName] -> IO [(HostName, SockAddr)]
resolveAddrs xs = concat . zipWith (zip . repeat) xs <$> mapM ntpAddrs xs

main :: IO ()
main = withSocketsDo $ do
    --putStrLn "NTP Monitor"
    addrs <- resolveAddrs servers

    bracket ntpSocket sClose $ \sock -> do
    forever $ do
        mapM_ (roundtrip sock) addrs
        threadDelay 1000000
  where
    roundtrip :: Socket -> (HostName, SockAddr) -> IO ()
    roundtrip sock (host, addr) = do
        ereply <- ntpSend sock addr >> ntpRecv sock
        case ereply of
            Left err    -> putStrLn ("Error: " ++ err)
            Right reply -> printCsv host addr reply

printCsv :: HostName -> SockAddr -> NTPReply -> IO ()
printCsv host addr reply = do
    now <- getCurrentTime
    index <- rdtsc

    let offset = localClockOffset reply
        delay  = roundtripDelay reply
        ntp    = offset `addUTCTime` now
        time   = formatTime defaultTimeLocale "%T.%q" ntp
        csv    = intercalate ","
                 [ host
                 --, show addr
                 , showMilli delay
                 , showMilli offset
                 , show index
                 , time ]

    putStrLn csv

------------------------------------------------------------------------

ntpSocket :: IO Socket
ntpSocket = socket AF_INET Datagram defaultProtocol

ntpAddrs :: HostName -> IO [SockAddr]
ntpAddrs host =
    map addrAddress <$> getAddrInfo hints (Just host) (Just "ntp")
  where
    hints = Just defaultHints { addrFamily     = AF_INET
                              , addrSocketType = Datagram }

ntpSend :: Socket -> SockAddr -> IO ()
ntpSend sock addr = do
    now <- getCurrentTime
    let msg = emptyNTPMsg { ntpTransmitTime = now }
        bs  = runPut (put msg)
    sendAllTo sock bs addr

ntpRecv :: Socket -> IO (Either String NTPReply)
ntpRecv sock = do
    bs <- recv sock 128
    now <- getCurrentTime
    return (NTPReply now <$> runGet get bs)

------------------------------------------------------------------------

data NTPReply = NTPReply UTCTime NTPMsg
    deriving (Show)

roundtripDelay :: NTPReply -> NominalDiffTime
roundtripDelay = go . timestamps
  where
    go (t1,t2,t3,t4) = (t4 `diffUTCTime` t1) - (t2 `diffUTCTime` t3)

localClockOffset :: NTPReply -> NominalDiffTime
localClockOffset = go . timestamps
  where
    go (t1,t2,t3,t4) = ((t2 `diffUTCTime` t1) + (t3 `diffUTCTime` t4)) / 2

timestamps :: NTPReply -> (UTCTime, UTCTime, UTCTime, UTCTime)
timestamps (NTPReply ntpDestinationTime NTPMsg {..}) =
    ( ntpOriginateTime   -- time request sent by client
    , ntpReceiveTime     -- time request received by server
    , ntpTransmitTime    -- time reply sent by server
    , ntpDestinationTime -- time reply received by client
    )

showMilli :: NominalDiffTime -> String
showMilli t = printf "%+.4fms" ms
  where
    ms = (1000 :: Double) * (realToFrac t)
