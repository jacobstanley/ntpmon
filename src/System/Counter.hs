{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module System.Counter (
      CounterInfo (..)
    , readCounter
    , analyzeCounter
    , analyzeFrequency
    , analyzePrecision
    ) where

import Control.Concurrent (threadDelay)
import Control.Monad (replicateM)
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import Data.Word (Word64)

#ifdef mingw32_HOST_OS
import System.Win32.Time (queryPerformanceCounter)
#else
import System.CPUTime.Rdtsc (rdtsc)
#endif

------------------------------------------------------------------------

-- | Information about the high precision counter.
data CounterInfo = CounterInfo {
      cntFrequency :: Word64
    , cntPrecision :: Word64
    } deriving (Show)

------------------------------------------------------------------------

-- | Reads the value of the best available high precision counter
-- on the system.
readCounter :: IO Word64

#ifdef mingw32_HOST_OS
readCounter = fromIntegral `fmap` queryPerformanceCounter
#else
readCounter = rdtsc
#endif


-- | Analyzes the high precision counter and returns its frequency and
-- precision.
analyzeCounter :: IO CounterInfo
analyzeCounter = do
    cntFrequency <- analyzeFrequency 5
    cntPrecision <- analyzePrecision
    return CounterInfo{..}

-- | Analyzes the frequency of the high precision counter and returns
-- the result in Hz (i.e. the amount the counter is incremented each
-- second.)
analyzeFrequency :: Int -- ^ the time (in seconds) to spend analyzing the counter
                 -> IO Word64
analyzeFrequency seconds = do
    t0 <- getCurrentTime
    c0 <- readCounter
    threadDelay (seconds * 1000000)
    t1 <- getCurrentTime
    c1 <- readCounter
    return $! rate c0 t0 c1 t1

-- | Determines the smallest possible increment of the high precision
-- counter. This effectively measures the overhead of the call to read
-- the counter.
analyzePrecision :: IO Word64
analyzePrecision = do
    cs <- replicateM 1000 readCounter
    return (prec cs)
  where
    diff (t0, t1) = t1 - t0
    prec xs = (minimum . filter (/= 0) . map diff) (zip xs (tail xs))

------------------------------------------------------------------------

rate :: Word64 -> UTCTime -> Word64 -> UTCTime -> Word64
rate c0 t0 c1 t1 = round (dc / dt :: Double)
  where
    dc = fromIntegral (c1 - c0)
    dt = realToFrac (t1 `diffUTCTime` t0)
