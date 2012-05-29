{-# LANGUAGE CPP #-}

module Win32Compat (getNTPConf) where

#ifdef mingw32_HOST_OS

import Control.Monad (filterM)
import System.Directory (doesFileExist)
import System.FilePath (takeDrive, normalise, (</>))
import System.Win32 (nullHANDLE, cSIDL_PROGRAM_FILES, sHGFP_TYPE_CURRENT)
import System.Win32 (sHGetFolderPath)

getNTPConf :: IO FilePath
getNTPConf = do
    pf <- getProgramFiles

    let conf     = "ntp/etc/ntp.conf"
        pfConf   = normalise (pf </> conf)
        rootConf = normalise (takeDrive pf </> conf)

    paths <- filterM doesFileExist [pfConf, rootConf]

    case paths of
        (path:_) -> return path
        []       -> error "getNTPConf: could not find ntp installation"

getProgramFiles :: IO String
getProgramFiles = sHGetFolderPath
    nullHANDLE cSIDL_PROGRAM_FILES nullHANDLE sHGFP_TYPE_CURRENT

#else

getNTPConf :: IO FilePath
getNTPConf = do
    let conf = "/etc/ntp.conf"
    ok <- doesFileExist conf
    if ok then return conf
          else error "getNTPConf: could not find ntp config file"

#endif
