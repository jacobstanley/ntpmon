{-# LANGUAGE CPP #-}

module System.Counter (
      readCounter
    , analyzeFrequency
    , analyzeFrequency'
    ) where

import Control.Concurrent (threadDelay)
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import Data.Word (Word64)

#ifdef mingw32_HOST_OS
import System.Win32.Time (queryPerformanceCounter)
#else
import System.CPUTime.Rdtsc (rdtsc)
#endif

------------------------------------------------------------------------

-- | Reads the value of the best available high resolution counter
-- on the system.
readCounter :: IO Word64

#ifdef mingw32_HOST_OS
readCounter = fromIntegral `fmap` queryPerformanceCounter
#else
readCounter = rdtsc
#endif


-- | Analyzes the frequency of the system's high resolution counter
-- and returns the result in Hz (i.e. the amount the counter is
-- incremented each second.)
analyzeFrequency :: IO Word64
analyzeFrequency = analyzeFrequency' 5

-- | Analyzes the frequency of the system's high resolution counter
-- and returns the result in Hz (i.e. the amount the counter is
-- incremented each second.)
analyzeFrequency' :: Int -- ^ the time (in seconds) to spend analyzing the counter
                 -> IO Word64
analyzeFrequency' seconds = do
    t0 <- getCurrentTime
    c0 <- readCounter
    threadDelay (seconds * 1000000)
    t1 <- getCurrentTime
    c1 <- readCounter
    return $! rate c0 t0 c1 t1

rate :: Word64 -> UTCTime -> Word64 -> UTCTime -> Word64
rate c0 t0 c1 t1 = round (dc / dt :: Double)
  where
    dc = fromIntegral (c1 - c0)
    dt = realToFrac (t1 `diffUTCTime` t0)
