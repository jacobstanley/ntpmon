{-# LANGUAGE RecordWildCards #-}

module System.Win32File (
      FileLogger
    , newDailyLogger
    , appendText
    ) where

import           Control.Monad (void)
import           Data.Bits ((.|.))
import qualified Data.ByteString as B
import           Data.ByteString.Internal (ByteString(..))
import           Data.IORef
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           Data.Time (UTCTime(..))
import           Data.Time.Calendar (Day(..), showGregorian)
import           Foreign.ForeignPtr (withForeignPtr)
import           Foreign.Ptr (plusPtr)
import           System.FilePath ((</>), (<.>))
import           System.Win32.File
import           System.Win32.Types

------------------------------------------------------------------------

data FileLogger = FileLogger {
      fileDir :: FilePath
    , fileRef :: IORef (Day, HANDLE)
    }

newDailyLogger :: FilePath -> IO FileLogger
newDailyLogger fileDir = do
    fileRef <- newIORef (ModifiedJulianDay 0, nullHANDLE)
    return FileLogger{..}

appendText :: FileLogger -> UTCTime -> T.Text -> IO ()
appendText FileLogger{..} (UTCTime day' _) text = do
    (_, h) <- modifyIORef' fileRef update
    writeBS h (encodeUtf8 text)
  where
    update (day, h)
        | day == day' = return (day, h)
        | otherwise   = do
            tryCloseHandle h
            h' <- openOrCreateFile (fileDir </> showGregorian day' <.> "csv")
            return (day', h')

writeBS :: HANDLE -> B.ByteString -> IO ()
writeBS _ (PS _  _ 0) = return ()
writeBS h (PS ps s n) = withForeignPtr ps write
  where
    -- TODO: This ignores the returned error code
    write p = void $ win32_WriteFile h (p `plusPtr` s) (fromIntegral n) Nothing

openOrCreateFile :: FilePath -> IO HANDLE
openOrCreateFile path = do
    h <- createFile path fILE_APPEND_DATA (fILE_SHARE_READ .|. fILE_SHARE_DELETE)
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

modifyIORef' :: IORef a -> (a -> IO a) -> IO a
modifyIORef' ref io = do
    x  <- readIORef ref
    x' <- io x
    writeIORef ref x'
    return x'
