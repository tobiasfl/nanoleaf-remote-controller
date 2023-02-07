{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Config
    ( getConfig
    , saveConfig
    , Config )
    where

import Data.Aeson
import Data.Aeson.TH
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as B
import AppMonad (EnvConfig, AppMonad, AppError (ConfigFileReadError, JSONDecodeError))
import Data.Text (Text)
import Data.Bifunctor (bimap, first, second)
import Control.Exception (try)
import Types (AuthToken, mkAuthToken)

data ConfigFile = ConfigFile 
    { authenticationToken :: Maybe Text
    }
    deriving (Show)

$(deriveJSON defaultOptions ''ConfigFile)

data Config = Config
    { configAuthTok :: Maybe AuthToken }
    deriving (Show)

readConfigFile :: FilePath -> IO (Either AppError B.ByteString)
readConfigFile fn = fmap (first ConfigFileReadError ) $ try $ B.readFile fn

getConfig :: FilePath -> IO (Either AppError Config)
getConfig fn = do
    confFileContentOrErr <- readConfigFile fn
    let decodedConfFileOrErr = confFileContentOrErr >>= first JSONDecodeError . eitherDecode 
    return (second (Config . fmap mkAuthToken . authenticationToken) decodedConfFileOrErr)

writeConfig :: FilePath -> ByteString -> AppMonad ()
writeConfig = undefined

saveConfig :: FilePath -> EnvConfig -> AppMonad ()
saveConfig = undefined
