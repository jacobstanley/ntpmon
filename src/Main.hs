{-# LANGUAGE ScopedTypeVariables #-}

--module Main (main) where
module Main where

import Control.Applicative ((<$>))
import Control.Exception (bracket)
import Data.Serialize
import Data.Time.Calendar
import Data.Time.Clock
import Data.Word (Word32)
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

import Data.NTP

------------------------------------------------------------------------

main :: IO ()
main = withSocketsDo $ do
    putStrLn "NTP Monitor"

    let servers = ["0.au.pool.ntp.org", "localhost"]
    addrs <- concat <$> mapM ntpAddr servers
    print addrs

    bracket ntpSocket sClose $ \sock -> do
    mapM_ (roundtrip sock) addrs
  where
    roundtrip :: Socket -> SockAddr -> IO ()
    roundtrip sock addr = do
        emsg <- ntpSend sock addr >> ntpRecv sock
        case emsg of
            Left err  -> putStrLn ("Error: " ++ err)
            Right msg -> print msg

------------------------------------------------------------------------

ntpSocket :: IO Socket
ntpSocket = socket AF_INET Datagram defaultProtocol

ntpAddr :: HostName -> IO [SockAddr]
ntpAddr host =
    map addrAddress <$> getAddrInfo hints (Just host) (Just "ntp")
  where
    hints = Just defaultHints { addrFamily     = AF_INET
                              , addrSocketType = Datagram }

ntpSend :: Socket -> SockAddr -> IO ()
ntpSend sock addr = do
    now <- ntpCurrentTime
    let msg = emptyMsg { ntpTransmitTime = now }
        bs  = runPut (put msg)
    sendAllTo sock bs addr

ntpRecv :: Socket -> IO (Either String NTPMsg)
ntpRecv sock = runGet get <$> recv sock 128

------------------------------------------------------------------------

emptyMsg :: NTPMsg
emptyMsg = NTPMsg NoWarning Version4 Client 0 0 0 0 0 0
                  ntpJan1900 ntpJan1900 ntpJan1900 ntpJan1900

ntpJan1900 :: NTPTime
ntpJan1900 = NTPTime 0 0

utcJan1900 :: UTCTime
utcJan1900 = UTCTime (fromGregorian 1900 1 1) 0

ntpCurrentTime :: IO NTPTime
ntpCurrentTime = utc2ntp <$> getCurrentTime

utc2ntp :: UTCTime -> NTPTime
utc2ntp = diff2ntp . (`diffUTCTime` utcJan1900)

diff2ntp :: NominalDiffTime -> NTPTime
diff2ntp t = NTPTime int (truncate (frac * maxBound32))
  where
    (int, frac) = properFraction t

ntp2diff :: NTPTime -> NominalDiffTime
ntp2diff (NTPTime int frac) = fromIntegral int + (fromIntegral frac / maxBound32)

maxBound32 :: NominalDiffTime
maxBound32 = fromIntegral (maxBound :: Word32)
