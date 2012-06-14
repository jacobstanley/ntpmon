{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.NTP.ConfigFinder (findConfig) where

import System.Directory (doesFileExist)

#ifdef mingw32_HOST_OS

import Control.Exception (IOException, handle, bracket)
import Control.Monad (when)
import Data.List (find, tails, isPrefixOf)
import Foreign (withForeignPtr, nullPtr, castPtr, peek)
import Foreign.Marshal (alloca, allocaArray0, maybeWith)
import System.Win32 (HKEY, hKEY_LOCAL_MACHINE)
import System.Win32 (rEG_SZ, rEG_MULTI_SZ, rEG_EXPAND_SZ)
import System.Win32 (regOpenKey, regCloseKey, c_RegQueryValueEx)
import System.Win32 (withTString, peekTString, failUnlessSuccess)

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
                            (flip regQueryValueString value)

    hive  = hKEY_LOCAL_MACHINE
    key   = "SYSTEM\\CurrentControlSet\\Services\\ntp"
    value = Just "ImagePath"

    findConf :: String -> Maybe String
    findConf = fmap (takePath . drop 3) . find ("-c " `isPrefixOf`) . tails

    takePath ('"':xs) = takeWhile (/= '"') xs
    takePath xs       = takeWhile (/= ' ') xs

    handleIOError = handle $ \(e :: IOException) ->
        error $ "findConfig: could not read NTP service details\n" ++ show e

-- TODO [ Submit bug report to Win32 package ]
-- TODO 'System.Win32.Registry.regQueryValue' throws if the key type is
-- TODO not REG_SZ, even though REG_MULTI_SZ and REG_EXPAND_SZ are also
-- TODO valid string types. This function fixes that problem.
regQueryValueString :: HKEY -> Maybe String -> IO String
regQueryValueString key mb_subkey =
    withForeignPtr key $ \ p_key ->
    maybeWith withTString mb_subkey $ \ c_subkey ->
    alloca $ \ p_ty ->
    alloca $ \ p_value_len -> do
    failUnlessSuccess "RegQueryValue" $
        c_RegQueryValueEx p_key c_subkey nullPtr p_ty nullPtr p_value_len
    ty <- peek p_ty
    when (ty `notElem` [rEG_SZ, rEG_MULTI_SZ, rEG_EXPAND_SZ]) $
        ioError $ userError $ "RegQueryValue: expected value to be a string "
                           ++ "(REG_SZ, REG_MULTI_SZ or REG_EXPAND_SZ)"
    value_len <- peek p_value_len
    allocaArray0 (fromIntegral value_len) $ \ c_value -> do
        failUnlessSuccess "RegQueryValue" $
            c_RegQueryValueEx p_key c_subkey nullPtr p_ty c_value p_value_len
        peekTString (castPtr c_value)

#else

findPossibleConfig :: IO FilePath
findPossibleConfig = return "/etc/ntp.conf"

#endif
