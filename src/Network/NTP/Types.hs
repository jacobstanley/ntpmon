{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Most of the documentation for this module as been taken from
-- RFC-2030 (http://tools.ietf.org/html/rfc2030) which describes the
-- SNTP v4.
module Network.NTP.Types where

import Control.Applicative ((<$>), (<*>))
import Data.Bits ((.&.), shiftL, shiftR)
import Data.Serialize
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format ()
import Data.Word (Word8, Word32)

------------------------------------------------------------------------
-- Types

-- | This is a warning of an impending leap second to be inserted or
-- deleted in the last minute of the current day.
data LeapIndicator =
      NoWarning      -- ^ 0 - No warning
    | LastMinute61   -- ^ 1 - Last minute has 61 seconds
    | LastMinute59   -- ^ 2 - Last minute has 59 seconds
    | AlarmCondition -- ^ 3 - Alarm condition (clock not synchronized)
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
      SymmetricActive  -- ^ 1 - Symmetric active
    | SymmetricPassive -- ^ 2 - Symmetric passive
    | Client           -- ^ 3 - Client
    | Server           -- ^ 4 - Server
    | Broadcast        -- ^ 5 - Broadcast
    deriving (Eq, Show)

-- | Indicates the stratum level of the local clock, with values defined
-- as follows:
--
-- [@0@]      Unspecified or unavailable.
--
-- [@1@]      Primary reference (e.g. radio clock, PPS).
--
-- [@2-15@]   Secondary reference (via NTP or SNTP).
--
-- [@16-255@] Reserved.
--
type Stratum = Word8

-- | This is a 32-bit bitstring identifying the particular reference
-- source. In the case of NTP Version 3 or Version 4 stratum-0
-- (unspecified) or stratum-1 (primary) servers, this is a
-- four-character ASCII string, left justified and zero padded to 32
-- bits. In NTP Version 3 secondary servers, this is the 32-bit IPv4
-- address of the reference source. In NTP Version 4 secondary servers,
-- this is the low order 32 bits of the latest transmit timestamp of the
-- reference source.
--
-- NTP primary (stratum 1) servers should set this field to a code
-- identifying the external reference source according to the following
-- list:
--
-- [@LOCL@] Uncalibrated local clock used as a primary reference
--          for a subnet without external means of synchronization.
--
-- [@PPS@]  Atomic clock or other pulse-per-second source
--          individually calibrated to national standards.
--
-- [@ACTS@] NIST dialup modem service.
--
-- [@USNO@] USNO modem service.
--
-- [@PTB@]  PTB (Germany) modem service.
--
-- [@TDF@]  Allouis (France) Radio 164 kHz.
--
-- [@DCF@]  Mainflingen (Germany) Radio 77.5 kHz.
--
-- [@MSF@]  Rugby (UK) Radio 60 kHz.
--
-- [@WWV@]  Ft. Collins (US) Radio 2.5, 5, 10, 15, 20 MHz.
--
-- [@WWVB@] Boulder (US) Radio 60 kHz.
--
-- [@WWVH@] Kaui Hawaii (US) Radio 2.5, 5, 10, 15 MHz.
--
-- [@CHU@]  Ottawa (Canada) Radio 3330, 7335, 14670 kHz.
--
-- [@LORC@] LORAN-C radionavigation system.
--
-- [@OMEG@] OMEGA radionavigation system.
--
-- [@GPS@]  Global Positioning System.
--
-- [@GOES@] Geostationary Orbit Environment Satellite.
--
-- If the external reference is one of those listed, the associated
-- code should be used. Codes for sources not listed can be contrived as
-- appropriate.
type ReferenceId = Word32

-- | An NTP message.
data NTPMsg = NTPMsg {
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
    , ntpReferenceTime :: UTCTime

    -- | /Originate Time/ The time at which the request departed the client for
    -- the server.
    , ntpT1 :: UTCTime

    -- | /Receive Time/ The time at which the request arrived at the server.
    , ntpT2 :: UTCTime

    -- | /Transmit Time/ The time at which the reply departed the server for
    -- the client.
    , ntpT3 :: UTCTime

    } deriving (Eq, Show)

------------------------------------------------------------------------
-- Functions

emptyNTPMsg :: NTPMsg
emptyNTPMsg = NTPMsg NoWarning Version4 Client 0 0 0 0 0 0
              ntpOrigin ntpOrigin ntpOrigin ntpOrigin

------------------------------------------------------------------------
-- Serialize

instance Serialize NTPMsg where
    put NTPMsg{..} = do
        putLVM      ntpLeapIndicator ntpVersion ntpMode
        putWord8    ntpStratum
        putWord8    ntpPoll
        putWord8    ntpPrecision
        putWord32be ntpRootDelay
        putWord32be ntpRootDispersion
        putWord32be ntpReferenceId
        putUTCTime  ntpReferenceTime
        putUTCTime  ntpT1
        putUTCTime  ntpT2
        putUTCTime  ntpT3
    get = do
        (ntpLeapIndicator, ntpVersion, ntpMode) <- getLVM
        ntpStratum        <- getWord8
        ntpPoll           <- getWord8
        ntpPrecision      <- getWord8
        ntpRootDelay      <- getWord32be
        ntpRootDispersion <- getWord32be
        ntpReferenceId    <- getWord32be
        ntpReferenceTime  <- getUTCTime
        ntpT1             <- getUTCTime
        ntpT2             <- getUTCTime
        ntpT3             <- getUTCTime
        return NTPMsg{..}

------------------------------------------------------------------------
-- Serializing Timestamps

-- | An NTP timestamp. On the wire, timestamps are represented as a
-- 64-bit unsigned fixed-point number, in seconds relative to 1 January
-- 1900. The integer part is in the first 32 bits and the fraction part
-- in the last 32 bits.
data NTPTime = NTPTime !Word32 !Word32
    deriving (Eq, Show)

putUTCTime :: Putter UTCTime
putUTCTime t = putWord32be int >> putWord32be frac
  where
    (NTPTime int frac) = fromUTC t

getUTCTime :: Get UTCTime
getUTCTime = toUTC <$> (NTPTime <$> getWord32be <*> getWord32be)

fromUTC :: UTCTime -> NTPTime
fromUTC = diff2ntp . (`diffUTCTime` ntpOrigin)
  where
    diff2ntp :: NominalDiffTime -> NTPTime
    diff2ntp t = NTPTime int (truncate (frac * maxBound32))
      where
        (int, frac) = properFraction t

toUTC :: NTPTime -> UTCTime
toUTC = (`addUTCTime` ntpOrigin) . ntp2diff
  where
    ntp2diff :: NTPTime -> NominalDiffTime
    ntp2diff (NTPTime int frac) = fromIntegral int + (fromIntegral frac / maxBound32)

ntpOrigin :: UTCTime
ntpOrigin = UTCTime (fromGregorian 1900 1 1) 0

maxBound32 :: NominalDiffTime
maxBound32 = fromIntegral (maxBound :: Word32)

------------------------------------------------------------------------
-- Serializing the leap indicator / version / mode byte.

putLVM :: LeapIndicator -> Version -> Mode -> Put
putLVM l v m = putWord8 (packLVM l v m)

getLVM :: Get (LeapIndicator, Version, Mode)
getLVM = do
    lvm <- getWord8
    case unpackLVM lvm of
        (Just l, Just v, Just m) -> return (l,v,m)
        (Nothing, _, _) -> fail "invalid leap indicator"
        (_, Nothing, _) -> fail "unknown NTP version"
        (_, _, Nothing) -> fail "unknown message mode"

packLVM :: LeapIndicator -> Version -> Mode -> Word8
packLVM l v m = (fromLeap l `shiftL` 6)
              + (fromVersion v `shiftL` 3)
              + fromMode m

unpackLVM :: Word8 -> (Maybe LeapIndicator, Maybe Version, Maybe Mode)
unpackLVM x = (l, v, m)
  where
    l = toLeap (x `shiftR` 6) -- bits 1-2
    v = toVersion ((x .&. 0x38) `shiftR` 3) -- bits 3-5
    m = toMode (x .&. 0x7) -- bits 6-8

fromLeap :: LeapIndicator -> Word8
fromLeap x = case x of
    NoWarning      -> 0
    LastMinute61   -> 1
    LastMinute59   -> 2
    AlarmCondition -> 3

toLeap :: Word8 -> Maybe LeapIndicator
toLeap x = case x of
    0 -> Just NoWarning
    1 -> Just LastMinute61
    2 -> Just LastMinute59
    3 -> Just AlarmCondition
    _ -> Nothing

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
    SymmetricActive  -> 1
    SymmetricPassive -> 2
    Client           -> 3
    Server           -> 4
    Broadcast        -> 5

toMode :: Word8 -> Maybe Mode
toMode x = case x of
    1 -> Just SymmetricActive
    2 -> Just SymmetricPassive
    3 -> Just Client
    4 -> Just Server
    5 -> Just Broadcast
    _ -> Nothing

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
