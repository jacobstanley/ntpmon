{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.NTP.Config (
    -- * Types
      ServerConfig (..)
    , ServerMode (..)

    -- * Reading / Writing
    , readConfig
    , writeConfig
    ) where

import           Control.Applicative ((<$>))
import qualified Data.Text as T
import qualified Data.Text.IO as T

------------------------------------------------------------------------
-- Types

data ServerConfig = ServerConfig {
      cfgHostName :: T.Text
    , cfgMode     :: ServerMode
    }

data ServerMode = Prefer | Normal | NoSelect

------------------------------------------------------------------------
-- Reading / Writing

readConfig :: FilePath -> IO [ServerConfig]
readConfig path = servers <$> T.readFile path
  where
    servers = map mkServer
            . filter (not . null)
            . map (drop 1 . T.words)
            . filter ("server" `T.isPrefixOf`)
            . map T.stripStart
            . T.lines

    mkServer [] = error "readConfig: tried to decode blank server entry"
    mkServer (x:xs)
        | elem "prefer" xs   = cfg { cfgMode = Prefer }
        | elem "noselect" xs = cfg { cfgMode = NoSelect }
        | otherwise          = cfg
      where
        cfg = ServerConfig { cfgHostName = x
                           , cfgMode     = Normal }

writeConfig :: [ServerConfig] -> FilePath -> IO ()
writeConfig servers path = do
    file <- T.readFile path
    T.writeFile path (update file)
  where
    update = T.unlines . updateLines . map T.stripStart . T.lines

    updateLines xs =
        let (ys, zs) = break isServer xs
        in ys ++ map mkText servers ++ filter (not . isServer) zs

    nameWidth = maximum $ map (T.length . cfgHostName) servers
    padding xs = T.replicate (nameWidth - T.length xs) " "

    mkText ServerConfig{..} = T.concat
        [ "server "
        , cfgHostName
        , padding cfgHostName
        , " minpoll 3"
        , " maxpoll 3"
        , " iburst"
        , case cfgMode of
            Prefer   -> " prefer"
            NoSelect -> " noselect"
            _        -> ""
        ]

    isServer = ("server" `T.isPrefixOf`)
