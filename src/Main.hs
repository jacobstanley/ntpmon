{-# LANGUAGE ScopedTypeVariables #-}

--module Main (main) where
module Main where

import Control.Applicative ((<$>))
import Control.Exception (bracket)
import Data.Serialize
import Data.Time.Clock
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
    now <- getCurrentTime
    let msg = emptyNTPMsg { ntpTransmitTime = now }
        bs  = runPut (put msg)
    sendAllTo sock bs addr

ntpRecv :: Socket -> IO (Either String NTPMsg)
ntpRecv sock = runGet get <$> recv sock 128
