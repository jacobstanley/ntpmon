{-# LANGUAGE CPP #-}

module System.Counter (readCounter) where

import Data.Word (Word64)

#ifdef mingw32_HOST_OS
import Control.Applicative ((<$>))
import System.Win32.Time (queryPerformanceCounter)
#else
import System.CPUTime.Rdtsc (rdtsc)
#endif

------------------------------------------------------------------------

readCounter :: IO Word64

#ifdef mingw32_HOST_OS
readCounter = fromIntegral <$> queryPerformanceCounter
#else
readCounter = rdtsc
#endif
