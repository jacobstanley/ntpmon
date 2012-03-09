{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.NTP where

import           Control.Applicative ((<$>))
import           Control.Exception (IOException, bracket, handle)
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified Data.ByteString as B
import           Data.Serialize
import           Data.Time.Clock
import           Data.Word (Word64)
import           Network.Socket hiding (send, sendTo, recv, recvFrom)
import           Network.Socket.ByteString
import           System.Timeout (timeout)

import           Network.NTP.Types hiding (Server)
import           System.Counter (readCounter)

------------------------------------------------------------------------
-- Types

newtype NTP a = NTP { runNTP :: ReaderT NTPData IO a }
    deriving (Monad, MonadIO, MonadReader NTPData)

data NTPData = NTPData {
      ntpSocket :: Socket
    }

withNTP :: NTP a -> IO a
withNTP = withSocketsDo . bracket create destroy . runReaderT . runNTP
  where
    create = do
        ntpSocket <- socket AF_INET Datagram defaultProtocol
        return NTPData{..}

    destroy NTPData{..} = sClose ntpSocket

------------------------------------------------------------------------

data Server = Server {
      svrHostName :: HostName
    , svrAddress  :: SockAddr
    , svrRecords  :: [Record]
    } deriving (Show)

type Counter = Word64

type Record = (Counter, UTCTime)

svrName :: Server -> String
svrName svr | host /= addr = host ++ " (" ++ addr ++ ")"
            | otherwise    = host
  where
    host = svrHostName svr
    addr = (takeWhile (/= ':') . show . svrAddress) svr

------------------------------------------------------------------------

-- | Resolves a list of IP addresses registered for the specified
-- hostname and creates 'Server' instances for each of them.
resolveServers :: HostName -> IO [Server]
resolveServers host =
    map (mkServer . addrAddress) <$> getHostAddrInfo
  where
    mkServer addr   = Server host addr []
    getHostAddrInfo = getAddrInfo hints (Just host) (Just "ntp")

    hints = Just defaultHints { addrFamily     = AF_INET
                              , addrSocketType = Datagram }

updateServer :: Server -> NTP Server
updateServer svr = do
    t1 <- transmit (svrAddress svr)
    rs <- toList =<< receive t1
    return (insertRecords svr rs)
  where
    toList (Left _)  = return []
    toList (Right x) = return [x]

insertRecords :: Server -> [Record] -> Server
insertRecords svr xs = svr { svrRecords = records }
  where
    records = take 5 (xs ++ svrRecords svr)

transmit :: SockAddr -> NTP Counter
transmit addr = do
    NTPData{..} <- ask
    liftIO $ do
        now <- getCurrentTime
        let msg = emptyNTPMsg { ntpTransmitTime = now }
            bs  = runPut (put msg)
        count <- (B.length bs) `seq` readCounter
        sendAllTo ntpSocket bs addr
        return count

receive :: Counter -> NTP (Either String Record)
receive t1 = do
    NTPData{..} <- ask
    liftIO $ do
        mbs <- (handleIOErrors . timeout 1000000 . recv ntpSocket) 128
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
