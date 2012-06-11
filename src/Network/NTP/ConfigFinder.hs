{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.NTP.ConfigFinder (findConfig) where

import System.Directory (doesFileExist)

#ifdef mingw32_HOST_OS

import Control.Exception (IOException, handle, bracket)
import Data.List (find, tails, isPrefixOf)
import System.Win32 (hKEY_LOCAL_MACHINE, regOpenKey, regCloseKey, regQueryValue)

#endif

------------------------------------------------------------------------

findConfig :: IO FilePath
findConfig = do
    conf <- findPossibleConfig
    ok   <- doesFileExist conf
    if ok then return conf
          else error $ "findConfig: " ++ conf ++ " does not exist"

#ifdef mingw32_HOST_OS

findPossibleConfig :: IO FilePath
findPossibleConfig = do
    cmd <- handleIOError getServiceCmd
    case findConf cmd of
      Just conf -> return conf
      Nothing   -> error $ "findConfig: could not find 'ntp.conf' location\n"
                        ++ "NTP service command: " ++ cmd
  where
    getServiceCmd = bracket (regOpenKey hive key) regCloseKey
                            (flip regQueryValue value)

    hive  = hKEY_LOCAL_MACHINE
    key   = "SYSTEM\\CurrentControlSet\\Services\\ntp"
    value = Just "ImagePath"

    findConf :: String -> Maybe String
    findConf = fmap (takePath . drop 3) . find ("-c " `isPrefixOf`) . tails

    takePath ('"':xs) = takeWhile (/= '"') xs
    takePath xs       = takeWhile (/= ' ') xs

    handleIOError = handle (\(_ :: IOException) ->
        error "findConfig: NTP service is not installed on this computer")

#else

findPossibleConfig :: IO FilePath
findPossibleConfig = return "/etc/ntp.conf"

#endif
