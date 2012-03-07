{-# LANGUAGE CPP #-}

module System.Clock (ClockCount, getClockCount) where

import Control.Applicative ((<$>))
import Data.Word (Word64)

#ifdef mingw32_HOST_OS
import System.Win32.Time (queryPerformanceCounter)
#else
import System.CPUTime.Rdtsc (rdtsc)
#endif

------------------------------------------------------------------------

type ClockCount = Word64

getClockCount :: IO ClockCount

#ifdef mingw32_HOST_OS
getClockCount = fromIntegral <$> queryPerformanceCounter
#else
getClockCount = rdtsc
#endif
