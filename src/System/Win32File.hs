{-# LANGUAGE RecordWildCards #-}

module System.Win32File (
      RecordLogger
    , newRecordLogger
    , writeRecord
    ) where

import           Control.Monad (void)
import           Data.Bits ((.|.))
import qualified Data.ByteString as B
import           Data.ByteString.Internal (ByteString(..))
import           Data.IORef
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           Data.Time
import           Foreign.ForeignPtr (withForeignPtr)
import           Foreign.Ptr (plusPtr)
import           System.Directory (doesFileExist)
import           System.FilePath ((</>), (<.>))
import           System.Win32.File
import           System.Win32.Types

------------------------------------------------------------------------

type Headers = T.Text
type Record  = T.Text

data RecordLogger = RecordLogger {
      logDir      :: FilePath
    , logFile     :: IORef File
    , logInterval :: NominalDiffTime
    }

data File = File {
      fileHandle  :: HANDLE
    , fileTime    :: UTCTime -- time of the last record
    , fileHeaders :: Headers
    }

newRecordLogger :: FilePath -> IO RecordLogger
newRecordLogger logDir = do
    logFile <- newIORef (File nullHANDLE utc0 T.empty)
    return RecordLogger{..}
  where
    logInterval = 10

utc0 :: UTCTime
utc0 = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)

writeRecord :: RecordLogger -> UTCTime -> Headers -> Record -> IO ()
writeRecord RecordLogger{..} time headers record = do
    mfile <- modifyIORefIO logFile updateFile
    case mfile of
        Just f  -> writeUtf8 (fileHandle f) record
        Nothing -> return ()
  where
    updateFile file@File{..} =
      case (timeOk, nameOk) of
        (False, _)    -> deny file
        (True, True)  -> allow file { fileTime = fileTime' }
        (True, False) -> do
            tryCloseHandle fileHandle
            path      <- nextFilePath (logDir </> fileName') "csv"
            newHandle <- openOrCreateFile path
            writeUtf8 newHandle headers
            allow (File newHandle fileTime' headers)
      where
        nameOk = time `dayEq` fileTime && headers == fileHeaders
        timeOk = fileTime /= fileTime'

        fileName' = showGregorian (utctDay time)
        fileTime' = time { utctDayTime = dayTime' }

        dayTime'  = fromIntegral intervals * interval
        intervals = floor (utctDayTime time / interval) :: Int
        interval  = realToFrac logInterval

        allow f = return (f, Just f)
        deny  f = return (f, Nothing)

dayEq :: UTCTime -> UTCTime -> Bool
dayEq (UTCTime d1 _) (UTCTime d2 _) = d1 == d2

nextFilePath :: FilePath -> String -> IO FilePath
nextFilePath base ext = go (0 :: Int)
  where
    go n = do
        let path = base ++ suffix n <.> ext
        exists <- doesFileExist path
        if exists then go (n+1)
                  else return path

    suffix 0 = ""
    suffix n = "_" ++ show n


writeUtf8 :: HANDLE -> T.Text -> IO ()
writeUtf8 h t = writeBS h (encodeUtf8 t)

writeBS :: HANDLE -> B.ByteString -> IO ()
writeBS _ (PS _  _ 0) = return ()
writeBS h (PS ps s n) = withForeignPtr ps write
  where
    -- TODO: This ignores the returned error code
    write p = void $ win32_WriteFile h (p `plusPtr` s) (fromIntegral n) Nothing

openOrCreateFile :: FilePath -> IO HANDLE
openOrCreateFile path = do
    h   <- createFile path fILE_APPEND_DATA (fILE_SHARE_READ .|. fILE_SHARE_DELETE)
                      Nothing oPEN_ALWAYS fILE_ATTRIBUTE_NORMAL Nothing
    err <- getLastError
    if err /= 0 && err /= 183
       then failWith "openOrCreateFile" err
       else return h

tryCloseHandle :: HANDLE -> IO ()
tryCloseHandle h | h == nullHANDLE = return ()
                 | otherwise       = closeHandle h

------------------------------------------------------------------------
-- Utils

fILE_APPEND_DATA :: AccessMode
fILE_APPEND_DATA = 4

fILE_SHARE_DELETE :: ShareMode
fILE_SHARE_DELETE = 4

modifyIORefIO :: IORef a -> (a -> IO (a, b)) -> IO b
modifyIORefIO ref io = do
    x  <- readIORef ref
    (x',r) <- io x
    writeIORef ref x'
    return r
