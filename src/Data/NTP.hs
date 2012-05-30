{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

-- | Most of the documentation for this module as been taken from
-- RFC-5905 (http://tools.ietf.org/html/rfc5905) which describes NTPv4.
module Data.NTP (
    -- * Types
      LeapIndicator (..)
    , Version (..)
    , Mode (..)
    , Stratum
    , ReferenceId
    , Timestamp (..)
    , Packet (..)

    -- * 'Packet' functions
    , requestMsg

    -- * 'Timestamp' functions
    , TimestampConvert (..)
    , addSeconds
    , diffSeconds
    , fromSeconds
    , toSeconds
    , packTimestamp
    , unpackTimestamp
    , ntpOrigin
    ) where

import Control.Applicative ((<$>))
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.Serialize
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format ()
import Data.Word (Word8, Word32, Word64)

------------------------------------------------------------------------
-- Types

-- | This is a warning of an impending leap second to be inserted or
-- deleted in the last minute of the current day.
data LeapIndicator =
      NoWarning    -- ^ 0 - No warning
    | LastMinute61 -- ^ 1 - Last minute of the day has 61 seconds
    | LastMinute59 -- ^ 2 - Last minute of the day has 59 seconds
    | Unknown      -- ^ 3 - Unknown (clock unsynchronized)
    deriving (Eq, Show)

-- | The version number is 3 for Version 3 (IPv4 only) and 4 for Version
-- 4 (IPv4, IPv6 and OSI). If necessary to distinguish between IPv4,
-- IPv6 and OSI, the encapsulating context must be inspected.
data Version =
      Version3 -- ^ 3 - Version 3 (IPv4 only)
    | Version4 -- ^ 4 - Version 4 (IPv4, IPv6 and OSI)
    deriving (Eq, Show)

-- | In unicast and anycast modes, the client should use the 'Client'
-- mode in the request, and the server should use the 'Server' mode in
-- the reply. In multicast mode, the server should use the 'Broadcast'
-- mode.
data Mode =
      Reserved         -- ^ 0 - Reserved
    | SymmetricActive  -- ^ 1 - Symmetric active
    | SymmetricPassive -- ^ 2 - Symmetric passive
    | Client           -- ^ 3 - Client
    | Server           -- ^ 4 - Server
    | Broadcast        -- ^ 5 - Broadcast
    | Control          -- ^ 6 - NTP control message
    | Private          -- ^ 7 - Reserved for private use
    deriving (Eq, Show)

-- | Indicates the stratum level of the local clock, with values defined
-- as follows:
--
-- [@0@]      Unspecified or invalid.
--
-- [@1@]      Primary reference (e.g. equipped with a GPS receiver).
--
-- [@2-15@]   Secondary reference (via NTP).
--
-- [@16@]     Unsynchronized.
--
-- [@17-255@] Reserved.
--
type Stratum = Word8

-- | 32-bit code identifying the particular server or reference clock.
-- The interpretation depends on the value in the stratum field.
--
-- For packet stratum 0 (unspecified or invalid), this is a
-- four-character ASCII string, called the "kiss code", used for
-- debugging and monitoring purposes.
--
-- For stratum 1 (reference clock), this is a four-octet,
-- left-justified, zero-padded ASCII string assigned to the reference
-- clock. The authoritative list of Reference Identifiers is maintained
-- by IANA; however, any string beginning with the ASCII character "X"
-- is reserved for unregistered experimentation and development. The
-- identifiers below have been used as ASCII identifiers:
--
-- [@GOES@] Geostationary Orbit Environment Satellite.
--
-- [@GPS@]  Global Positioning System.
--
-- [@GAL@]  Galileo Positioning System.
--
-- [@PPS@]  Generic pulse-per-second.
--
-- [@IRIG@] Inter-Range Instrumentation Group.
--
-- [@WWVB@] LF Radio WWVB Ft. Collins, CO 60 kHz
--
-- [@DCF@]  LF Radio DCF77 Mainflingen, DE 77.5 kHz
--
-- [@HBG@]  LF Radio HBG Prangins, HB 75 kHz
--
-- [@MSF@]  LF Radio MSF Anthorn, UK 60 kHz
--
-- [@JJY@]  LF Radio JJY Fukushima, JP 40 kHz, Saga, JP 60 kHz
--
-- [@LORC@] MF Radio LORAN C station, 100 kHz
--
-- [@TDF@]  MF Radio Allouis, FR 162 kHz
--
-- [@CHU@]  HF Radio CHU Ottawa, Ontario
--
-- [@WWV@]  HF Radio WWV Ft. Collins, CO
--
-- [@WWVH@] HF Radio WWVH Kauai, HI
--
-- [@NIST@] NIST telephone modem
--
-- [@ACTS@] NIST telephone modem
--
-- [@USNO@] USNO telephone modem
--
-- [@PTB@]  European telephone modem
--
-- Above stratum 1 (secondary servers and clients): this is the
-- reference identifier of the server and can be used to detect timing
-- loops. If using the IPv4 address family, the identifier is the
-- four-octet IPv4 address. If using the IPv6 address family, it is the
-- first four octets of the MD5 hash of the IPv6 address. Note that,
-- when using the IPv6 address family on an NTPv4 server with a NTPv3
-- client, the Reference Identifier field appears to be a random value
-- and a timing loop might not be detected.
type ReferenceId = Word32

-- | An NTP timestamp. On the wire, timestamps are represented as a
-- 64-bit unsigned fixed-point number, in seconds relative to 1 January
-- 1900. The integer part is in the first 32 bits and the fraction part
-- in the last 32 bits.
newtype Timestamp = Timestamp { unTimestamp :: Word64 }
    deriving (Eq, Show, Ord, Enum, Num, Real, Integral)

-- | An NTP data packet.
data Packet = Packet {
    -- | See 'LeapIndicator' for details.
      ntpLeapIndicator :: !LeapIndicator

    -- | See 'Version' for details.
    , ntpVersion :: !Version

    -- | See 'Mode' for details.
    , ntpMode :: !Mode

    -- | See 'Stratum' for details.
    , ntpStratum :: !Stratum

    -- | This is an 8-bit signed integer indicating the maximum interval
    -- between successive messages, in seconds to the nearest power of
    -- two. The values that can appear in this field presently range
    -- from 4 (16 s) to 14 (16284 s); however, most applications use
    -- only the sub-range 6 (64 s) to 10 (1024 s).
    , ntpPoll :: !Word8

    -- | This is an 8-bit signed integer indicating the precision of the
    -- local clock, in seconds to the nearest power of two.  The values
    -- that normally appear in this field range from -6 for
    -- mains-frequency clocks to -20 for microsecond clocks found in
    -- some workstations.
    , ntpPrecision :: !Word8

    -- | This is a 32-bit signed fixed-point number indicating the total
    -- roundtrip delay to the primary reference source, in seconds with
    -- fraction point between bits 15 and 16. Note that this variable
    -- can take on both positive and negative values, depending on the
    -- relative time and frequency offsets. The values that normally
    -- appear in this field range from negative values of a few
    -- milliseconds to positive values of several hundred milliseconds.
    , ntpRootDelay :: !Word32

    -- | This is a 32-bit unsigned fixed-point number indicating the
    -- nominal error relative to the primary reference source, in
    -- seconds with fraction point between bits 15 and 16. The values
    -- that normally appear in this field range from 0 to several
    -- hundred milliseconds.
    , ntpRootDispersion :: !Word32

    -- | See 'ReferenceId' for details.
    , ntpReferenceId :: !Word32

    -- | The time at which the local clock was last set or corrected.
    , ntpReferenceTime :: !Timestamp

    -- | /Originate Time/ The time at which the request departed the client for
    -- the server.
    , ntpT1 :: !Timestamp

    -- | /Receive Time/ The time at which the request arrived at the server.
    , ntpT2 :: !Timestamp

    -- | /Transmit Time/ The time at which the reply departed the server for
    -- the client.
    , ntpT3 :: !Timestamp

    } deriving (Eq, Show)

------------------------------------------------------------------------
-- NTP Messages

-- | A client message requesting the current time from a server.
requestMsg :: Timestamp -> Packet
requestMsg t3 = empty { ntpT3 = t3 }
  where
    empty = Packet NoWarning Version4 Client 0 0 0 0 0 0
            (Timestamp 0)
            (Timestamp 0)
            (Timestamp 0)
            (Timestamp 0)

------------------------------------------------------------------------
-- Timestamps

-- | Conversion to and from an NTP 'Timestamp'.
class TimestampConvert a where
    toTimestamp   :: a -> Timestamp
    fromTimestamp :: Timestamp -> a

instance TimestampConvert UTCTime where
    toTimestamp   = fromSeconds . (`diffUTCTime` ntpOrigin)
    fromTimestamp = (`addUTCTime` ntpOrigin) . toSeconds

-- | Calculates the difference between two timestamps in seconds.
addSeconds :: RealFrac a => Timestamp -> a -> Timestamp
addSeconds t s | s > 0     = t + fromSeconds s
               | otherwise = t - fromSeconds s

-- | Calculates the difference between two timestamps in seconds.
diffSeconds :: RealFrac a => Timestamp -> Timestamp -> a
diffSeconds t1 t2 | t1 > t2   = toSeconds (t1 - t2)
                  | otherwise = -toSeconds (t2 - t1)

-- | Converts from a fractional number of seconds to an NTP 'Timestamp'.
fromSeconds :: RealFrac a => a -> Timestamp
fromSeconds t = packTimestamp int (truncate (frac * fracsPerSecond))
  where
    (int, frac) = properFraction t

-- | Converts from an NTP 'Timestamp' to a fractional number of seconds.
toSeconds :: RealFrac a => Timestamp -> a
toSeconds t = fromIntegral int + (fromIntegral frac / fracsPerSecond)
  where
    (int, frac) = unpackTimestamp t

-- | Represents the number of fractional units in one second.
fracsPerSecond :: Real a => a
fracsPerSecond = fromIntegral (1 `shiftL` 32 :: Word64)

-- | Pack the 32-bit integer and fractional parts in to a 64-bit NTP 'Timestamp'.
packTimestamp :: Word32 -> Word32 -> Timestamp
packTimestamp int frac = Timestamp t
  where
    t = (fromIntegral int `shiftL` 32) .|. fromIntegral frac

-- | Unpack the 32-bit integer and fractional parts from a 64-bit NTP 'Timestamp'.
unpackTimestamp :: Timestamp -> (Word32, Word32)
unpackTimestamp (Timestamp t) = (int, frac)
  where
    int  = fromIntegral (t `shiftR` 32)
    frac = fromIntegral t

-- | The NTP origin, 1 January 1900, as a 'UTCTime'.
ntpOrigin :: UTCTime
ntpOrigin = UTCTime (fromGregorian 1900 1 1) 0

------------------------------------------------------------------------
-- Serialize

instance Serialize Packet where
    put Packet{..} = do
        putFlags    ntpLeapIndicator ntpVersion ntpMode
        putWord8    ntpStratum
        putWord8    ntpPoll
        putWord8    ntpPrecision
        putWord32be ntpRootDelay
        putWord32be ntpRootDispersion
        putWord32be ntpReferenceId
        put         ntpReferenceTime
        put         ntpT1
        put         ntpT2
        put         ntpT3
    get = do
        (ntpLeapIndicator, ntpVersion, ntpMode) <- getFlags
        ntpStratum        <- getWord8
        ntpPoll           <- getWord8
        ntpPrecision      <- getWord8
        ntpRootDelay      <- getWord32be
        ntpRootDispersion <- getWord32be
        ntpReferenceId    <- getWord32be
        ntpReferenceTime  <- get
        ntpT1             <- get
        ntpT2             <- get
        ntpT3             <- get
        return Packet{..}

instance Serialize Timestamp where
    put = putWord64be . unTimestamp
    get = Timestamp <$> getWord64be

------------------------------------------------------------------------
-- Serializing the NTP flags (leap indicator, version, mode)

putFlags :: LeapIndicator -> Version -> Mode -> Put
putFlags l v m = putWord8 (packFlags l v m)

getFlags :: Get (LeapIndicator, Version, Mode)
getFlags = do
    lvm <- getWord8
    case unpackFlags lvm of
        (l, Just v, m)  -> return (l,v,m)
        (_, Nothing, _) -> fail "unknown NTP version"

packFlags :: LeapIndicator -> Version -> Mode -> Word8
packFlags l v m = (fromLeap l `shiftL` 6)
                + (fromVersion v `shiftL` 3)
                + fromMode m

unpackFlags :: Word8 -> (LeapIndicator, Maybe Version, Mode)
unpackFlags x = (l, v, m)
  where
    l = toLeap (x `shiftR` 6) -- bits 1-2
    v = toVersion ((x .&. 0x38) `shiftR` 3) -- bits 3-5
    m = toMode (x .&. 0x7) -- bits 6-8

fromLeap :: LeapIndicator -> Word8
fromLeap x = case x of
    NoWarning    -> 0
    LastMinute61 -> 1
    LastMinute59 -> 2
    Unknown      -> 3

toLeap :: Word8 -> LeapIndicator
toLeap x = case x of
    0 -> NoWarning
    1 -> LastMinute61
    2 -> LastMinute59
    3 -> Unknown
    -- This should never happen as we don't export 'toLeap' and we only
    -- ever actually pass it a 2-bit word.
    _ -> error ("Network.NTP.Packet.toLeap: invalid leap indicator (" ++ show x ++ ")")

fromVersion :: Version -> Word8
fromVersion x = case x of
    Version3 -> 3
    Version4 -> 4

toVersion :: Word8 -> Maybe Version
toVersion x = case x of
    3 -> Just Version3
    4 -> Just Version4
    _ -> Nothing

fromMode :: Mode -> Word8
fromMode x = case x of
    Reserved         -> 0
    SymmetricActive  -> 1
    SymmetricPassive -> 2
    Client           -> 3
    Server           -> 4
    Broadcast        -> 5
    Control          -> 6
    Private          -> 7

toMode :: Word8 -> Mode
toMode x = case x of
    0 -> Reserved
    1 -> SymmetricActive
    2 -> SymmetricPassive
    3 -> Client
    4 -> Server
    5 -> Broadcast
    6 -> Control
    7 -> Private
    -- This should never happen as we don't export 'toMode' and we only
    -- ever actually pass it a 3-bit word.
    _ -> error ("Network.NTP.Packet.toMode: invalid mode (" ++ show x ++ ")")

------------------------------------------------------------------------
-- NTP Message Format
--
--   LI - Leap Indicator
--   VN - Version Number
--
--                        1                   2                   3
--    0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
--   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
--   |LI | VN  |Mode |    Stratum    |     Poll      |   Precision   |
--   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
--   |                          Root Delay                           |
--   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
--   |                       Root Dispersion                         |
--   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
--   |                     Reference Identifier                      |
--   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
--   |                                                               |
--   |                   Reference Timestamp (64)                    |
--   |                                                               |
--   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
--   |                                                               |
--   |                   Originate Timestamp (64)                    |
--   |                                                               |
--   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
--   |                                                               |
--   |                    Receive Timestamp (64)                     |
--   |                                                               |
--   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
--   |                                                               |
--   |                    Transmit Timestamp (64)                    |
--   |                                                               |
--   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
--   |                 Key Identifier (optional) (32)                |
--   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
--   |                                                               |
--   |                                                               |
--   |                 Message Digest (optional) (128)               |
--   |                                                               |
--   |                                                               |
--   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

------------------------------------------------------------------------
-- NTP Timestamp Format
--
--                        1                   2                   3
--    0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
--   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
--   |                           Seconds                             |
--   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
--   |                  Seconds Fraction (0-padded)                  |
--   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
--
