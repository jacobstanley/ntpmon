{-# LANGUAGE CPP #-}

module Win32Compat (getNTPConf) where

#ifdef mingw32_HOST_OS

import System.Win32 (sHGetFolderPath)
import System.Win32 (nullHANDLE, cSIDL_PROGRAM_FILES, sHGFP_TYPE_CURRENT)
import System.FilePath (normalise, (</>))

getNTPConf :: IO FilePath
getNTPConf = do
    pf <- getProgramFiles
    return (normalise (pf </> "ntp/etc/ntp.conf"))

getProgramFiles :: IO String
getProgramFiles = sHGetFolderPath
    nullHANDLE cSIDL_PROGRAM_FILES nullHANDLE sHGFP_TYPE_CURRENT

#else

getNTPConf :: IO FilePath
getNTPConf = return "/etc/ntp.conf"

#endif
