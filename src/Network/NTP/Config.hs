{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.NTP.Config (
    -- * Types
      ServerConfig (..)
    , Priority (..)
    , Driver (..)
    , HostName
    , SerialPort
    , BaudRate (..)
    , TimeOffset

    -- * Reading / Writing
    , readConfig
    , writeConfig
    ) where

import           Control.Applicative ((<$>))
import           Data.Bits ((.&.))
import           Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import           Prelude hiding (lines)

------------------------------------------------------------------------
-- Types

data ServerConfig = ServerConfig {
      cfgPriority :: Priority
    , cfgDriver   :: Driver
    } deriving (Show)

data Priority = Prefer | Normal | NoSelect
  deriving (Show)

type HostName = T.Text
type SerialPort = Int
type TimeOffset = Double

data Driver = UDP HostName | NMEA SerialPort BaudRate TimeOffset
  deriving (Show)

data BaudRate = B'4800 | B'9600 | B'19200 | B'38400 | B'57600 | B'115200
  deriving (Show)

------------------------------------------------------------------------
-- Reading

readConfig :: FilePath -> IO [ServerConfig]
readConfig path = parseConfig <$> T.readFile path

parseConfig :: T.Text -> [ServerConfig]
parseConfig txt = map fudges servers
  where
    servers = decode "server" server
    fudges  = foldl (.) id (decode "fudge" fudge)

    lines = map T.stripStart (T.lines txt)
    decode typ go = map go
                  $ filter (not . null)
                  $ map (drop 1 . T.words)
                  $ filter (typ `T.isPrefixOf`)
                  $ lines

server :: [T.Text] -> ServerConfig
server [] = error "server: tried to decode blank server entry"
server (host:mods)
    | elem "prefer" mods   = cfg { cfgPriority = Prefer }
    | elem "noselect" mods = cfg { cfgPriority = NoSelect }
    | otherwise            = cfg
  where
    cfg = ServerConfig Normal (driver host mods)

driver :: HostName -> [T.Text] -> Driver
driver host mods =
  case T.stripPrefix "127.127.20." host of
    Just x  -> NMEA (decimal x) (baud mods) 0
    Nothing -> UDP host
  where
    baud = pairwiseLookup "mode" B'4800 (decodeBaud . decimal)

type Fudge = ServerConfig -> ServerConfig

fudge :: [T.Text] -> Fudge
fudge []          cfg                 = cfg
fudge (host:mods) cfg | host /= host' = cfg
                      | otherwise     = cfg'
  where
    host'  = driverHost (cfgDriver cfg)
    cfg'   = cfg { cfgDriver = update (cfgDriver cfg) }

    update (NMEA p b _) = NMEA p b offset
    update x            = x

    offset = pairwiseLookup "time2" 0 double mods

-- | Reads a decimal number from text or returns 0 if it can't.
decimal :: T.Text -> Int
decimal = either (const 0) fst . T.decimal

-- | Reads a rational number from text or returns 0 if it can't.
double :: T.Text -> Double
double = either (const 0) fst . T.double

-- | Performs a pairwise lookup of key/value pairs.
pairwiseLookup :: T.Text -> a -> (T.Text -> a) -> [T.Text] -> a
pairwiseLookup key n f = go
  where
    go []                 = n
    go (k:x:_) | k == key = f x
    go (_:xs)             = go xs

------------------------------------------------------------------------
-- Writing

writeConfig :: [ServerConfig] -> FilePath -> IO ()
writeConfig servers path = do
    file <- T.readFile path
    T.writeFile path (update file)
  where
    update = T.unlines . updateLines . map T.stripStart . T.lines

    updateLines xs =
        let (ys, zs) = break hasServerConfig xs
        in ys
        ++ concatMap serverConfig servers
        ++ filter (not . hasServerConfig) zs

    hasServerConfig x = "server" `T.isPrefixOf` x ||
                        "fudge"  `T.isPrefixOf` x

    nameWidth = maximum $ map (T.length . driverText . cfgDriver) servers
    padding xs = T.replicate (nameWidth - T.length xs) " "

    serverConfig cfg = catMaybes [Just (serverText cfg), fudgeText cfg]

    serverText ServerConfig{..} = T.concat
        [ "server "
        , driverText cfgDriver
        , padding (driverText cfgDriver)
        , " minpoll 3"
        , " maxpoll 3"
        , " iburst"
        , case cfgPriority of
            Prefer   -> " prefer"
            NoSelect -> " noselect"
            _        -> ""
        ]

    driverText x = driverHost x `T.append` driverMode x

    fudgeText ServerConfig{..} = case cfgDriver of
        UDP _           -> Nothing
        NMEA _ _ offset -> Just $ T.concat
            [ "fudge  "
            , driverHost cfgDriver
            , " flag1 1"
            , " time2 ", tshow offset
            ]

driverHost :: Driver -> HostName
driverHost (UDP x)      = x
driverHost (NMEA n _ _) = "127.127.20." `T.append` tshow n

driverMode :: Driver -> T.Text
driverMode (UDP _)          = ""
driverMode (NMEA _ baud _ ) = " mode " `T.append` (tshow . encodeBaud) baud

tshow :: Show a => a -> T.Text
tshow = T.pack . show

------------------------------------------------------------------------
-- Utils

type Mode = Int

decodeBaud :: Mode -> BaudRate
decodeBaud x = case x .&. 0x70 of
    0x00 -> B'4800
    0x10 -> B'9600
    0x20 -> B'19200
    0x30 -> B'38400
    0x40 -> B'57600
    _    -> B'115200

encodeBaud :: BaudRate -> Mode
encodeBaud x = case x of
    B'4800   -> 0x00
    B'9600   -> 0x10
    B'19200  -> 0x20
    B'38400  -> 0x30
    B'57600  -> 0x40
    B'115200 -> 0x50
