{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get, put)
import Data.List (intercalate)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format
import System.Environment (getArgs)
import System.IO
import System.Locale (defaultTimeLocale)
import Text.Printf (printf)

import Network.NTP

import Control.Monad (forever)
import Control.Concurrent (forkIO)
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Foreign.C
import Foreign.Marshal
import Foreign.Storable
import Foreign.Ptr
import qualified Data.ByteString.Char8 as B

import Control.Concurrent.MVar
import Control.Exception (throw, Exception, fromException)
import Data.Typeable (Typeable)
import Network.HTTP.Types
import Network.Info
import Network.Wai
import Network.Wai.Handler.Warp
import qualified Data.ByteString.Lazy.Char8 as L

------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    case args of
      []            -> putStr usage
      ["--service"] -> do
        forkIO runService
        forever (threadDelay 10000000)
      hosts         -> withNTP (hPutStrLn stderr) $ do
        (reference:servers) <- resolve hosts
        monitor reference servers
  where
    resolve hosts = liftIO (concat <$> mapM resolveServers hosts)

usage :: String
usage = "NTP Monitor 0.5\n\
\Usage: ntpmon REFERENCE [SERVER]..\
\\n\
\\n  REFERENCE  The NTP server which the other servers will be measured\
\\n             against.\
\\n\
\\n  SERVER     An NTP server to monitor.\
\\n\
\\nNTP servers can be specified using either their hostname or IP address.\
\\n"

------------------------------------------------------------------------
-- Running as a service

--runService :: IO ()
--runService = withSocketsDo $ do
--    sock <- multicastReceiver "239.255.255.250" "1900"
--    forever $ do
--        (bs, addr) <- recvFrom sock 1024
--        sendAllTo sock bs addr
--
--        putStrLn (replicate 72 '-')
--        putStrLn ("Replied to " ++ show addr)
--        putStrLn (replicate 72 '-')
--        B.putStrLn bs

runService :: IO ()
runService = withSocketsDo $ do
    addrs <- getIPv4Addrs

    let app = ssdp "ntpmon" "http://0.0.0.0:45678"
    sock <- multicastReceiver addrs "239.255.255.250" "1900"
    runSettingsUdp settings sock app
  where
    settings = defaultSettings
        { settingsPort = 1900
        , settingsOnException = \e ->
            case fromException e of
                Just IgnoreRequest -> return ()
                Nothing -> settingsOnException defaultSettings e
        }

getIPv4Addrs :: IO [HostName]
getIPv4Addrs = map (show . ipv4) <$> getNetworkInterfaces

ssdp :: Ascii -> Ascii -> Application
ssdp serviceType location req = do
    liftIO $ putStrLn $ B.unpack method ++ " " ++ B.unpack st ++ " -> " ++ (show $ remoteHost req)
    case (method, path) of
        ("M-SEARCH", "*") | st == "ssdp:all"  -> ok
                          | st == serviceType -> ok
        _                                     -> ignore
  where
    method = requestMethod req
    path   = rawPathInfo req
    st     = maybe B.empty id (lookup "ST" (requestHeaders req))

    ok = return $ responseLBS status200
       [ ("Cache-Control", "max-age=1800")
       , ("ST", serviceType)
       , ("Location", location)
       , ("Server", serviceType `B.append` "/0.5")
       , ("Content-Length", "0")
       ] L.empty

    ignore = throw IgnoreRequest

data IgnoreRequest = IgnoreRequest
    deriving (Show, Typeable, Eq)
instance Exception IgnoreRequest

------------------------------------------------------------------------
-- Warp UDP Support

runSettingsUdp :: Settings -> Socket -> Application -> IO ()
runSettingsUdp set sock = runSettingsConnection set (getUdpConnection sock)

getUdpConnection :: Socket -> IO (Connection, SockAddr)
getUdpConnection sock = do
    (bs, addr) <- recvFrom sock (64 * 1024)
    req <- newMVar bs
    return (conn addr req, addr)
  where
    conn addr req = Connection
         { connSendMany = \xs -> sendManyTo sock xs addr
         , connSendAll  = \x  -> sendAllTo  sock x  addr
         , connSendFile = error "connSendFile: sendfile is not allowed with UDP connections"
         , connClose    = return ()
         , connRecv     = swapMVar req B.empty
         }

------------------------------------------------------------------------
-- Multicast Support

multicastReceiver :: [HostName] -> HostName -> ServiceName -> IO Socket
multicastReceiver localAddrs mcastAddr port = do
    (addr:_) <- getAddrInfo hints Nothing (Just port)

    sock <- socket (addrFamily addr)
                   (addrSocketType addr)
                   (addrProtocol addr)

    setSocketOption sock ReuseAddr 1

    bindSocket sock (addrAddress addr)
    mapM_ (addMembership sock mcastAddr) localAddrs

    return sock
  where
    hints = Just defaultHints
          { addrFlags      = [AI_PASSIVE, AI_ADDRCONFIG]
          , addrFamily     = AF_INET
          , addrSocketType = Datagram }

-- | Make the socket listen on multicast datagrams sent by the specified 'HostName'.
addMembership :: Socket -> HostName -> HostName -> IO ()
addMembership s mcast local = maybeIOError "addMembership" (doMulticastGroup _IP_ADD_MEMBERSHIP s mcast local)

maybeIOError :: String -> IO CInt -> IO ()
maybeIOError name f = f >>= \err -> case err of
    0 -> return ()
    _ -> ioError (errnoToIOError name (Errno (fromIntegral err)) Nothing Nothing)

doMulticastGroup :: CInt -> Socket -> HostName -> HostName -> IO CInt
doMulticastGroup flag (MkSocket s _ _ _ _) mcast local = allocaBytes (8) $ \mReqPtr -> do
    mcastAddr <- inet_addr mcast
    localAddr <- inet_addr local
    (\hsc_ptr -> pokeByteOff hsc_ptr 0) mReqPtr mcastAddr
    (\hsc_ptr -> pokeByteOff hsc_ptr 4) mReqPtr localAddr
    c_setsockopt s _IPPROTO_IP flag (castPtr mReqPtr) ((8))

foreign import stdcall unsafe "setsockopt"
    c_setsockopt :: CInt -> CInt -> CInt -> Ptr CInt -> CInt -> IO CInt

-- foreign import stdcall unsafe "WSAGetLastError"
--     wsaGetLastError :: IO CInt
-- 
-- getLastError :: CInt -> IO CInt
-- getLastError = const wsaGetLastError

_IP_MULTICAST_IF, _IP_MULTICAST_TTL, _IP_MULTICAST_LOOP, _IP_ADD_MEMBERSHIP, _IP_DROP_MEMBERSHIP :: CInt
_IP_MULTICAST_IF    = 9
_IP_MULTICAST_TTL   = 10
_IP_MULTICAST_LOOP  = 11
_IP_ADD_MEMBERSHIP  = 12
_IP_DROP_MEMBERSHIP = 13
_IPPROTO_IP :: CInt
_IPPROTO_IP = 0

------------------------------------------------------------------------

monitor :: Server -> [Server] -> NTP ()
monitor ref ss = do
    liftIO $ do
        hSetBuffering stdout LineBuffering
        (putStrLn . intercalate "," . map fst) headers
        (putStrLn . intercalate "," . map snd) headers

    monitorLoop ref ss
  where
    servers = ref:ss
    refName = svrHostName ref

    headers = [("Unix Time", "Seconds Since 1970"), ("UTC Time", "UTC Time")]
           ++ [("Filtered Offset", "(ms)")]
           ++ map (header "(ms)" (\x -> refName ++ " vs " ++ x)) servers
           ++ map (header "(ms)" (\x -> "Network Delay to " ++ x)) servers
           ++ map (header "(ms)" (\x -> refName ++ " vs " ++ x ++ " (error)")) servers
           ++ [ ("High Precision Counter Frequency", "(Hz)") ]
          -- ++ map (header "(ms)" (\x -> refName ++ " vs " ++ x ++ " (filtered)")) servers
          -- ++ map (header "(ms)" (\x -> "Network Delay to " ++ x ++ " (filtered)")) servers

    header unit mkname s = (mkname (svrHostName s), unit)


monitorLoop :: Server -> [Server] -> NTP ()
monitorLoop ref ss = do
    -- send requests to servers
    mapM transmit (ref:ss)

    -- wait for replies
    liftIO $ threadDelay $ round (1000000 / samplesPerSecond)

    -- update any servers which received replies
    (ref':ss') <- updateServers (ref:ss)

    -- sync clock with reference server
    mclock <- syncClockWith ref'

    -- check if we're synchronized
    liftIO $ case mclock of
        Nothing -> return ()
        Just (clock, off) -> do
            -- we are, so write samples to csv
            writeSamples clock (ref':ss') off

    monitorLoop ref' ss'

syncClockWith :: Server -> NTP (Maybe (Clock, Seconds))
syncClockWith server = do
    ntp@NTPData{..} <- get
    let mclock = adjustClock server ntpClock
    case mclock of
        Nothing         -> return ()
        Just (clock, _) -> put ntp { ntpClock = clock }
    return mclock

writeSamples :: Clock -> [Server] -> Seconds -> IO ()
writeSamples clock servers off = do
    utc <- getCurrentTime clock

    let utcTime  = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" utc
        unixTime = (init . show) (utcTimeToPOSIXSeconds utc)
        index    = [unixTime, utcTime]

    putStrLn (intercalate "," (index ++ fields))
  where
    fields = [offMs] ++ offsets ++ delays ++ errors ++ [freq]

    offMs = printf "%.9f" (off * 1000)
    freq = (show . clockFrequency) clock

    samples = map (head . svrRawSamples) servers
    offsets = map (showMilli . offset clock) samples
    delays  = map (showMilli . fromDiff clock . roundtrip) samples
    errors  = map (showMilli . (/10) . uncurry (currentError clock)) (zip servers samples)

showMilli :: Seconds -> String
showMilli t = printf "%.4f" ms
  where
    ms = (1000 :: Double) * (realToFrac t)
