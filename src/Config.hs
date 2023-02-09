{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Config
    ( getConfig
    , saveConfig
    , authenticationToken
    )
    where

import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString.Lazy as B
import AppMonad (EnvConfig (configAuthToken), AppMonad, AppError (ConfigFileReadError, ConfigFileWriteError, JSONDecodeError), liftIO)
import Data.Text (Text)
import Data.Bifunctor (bimap, first, second)
import Control.Exception (try, catch)
import Types (AuthToken (getAuthToken), mkAuthToken)
import Control.Monad.Except (throwError)
import Data.Foldable (forM_)

data ConfigFile = ConfigFile 
    { authenticationToken :: Maybe Text
    }
    deriving (Show)

$(deriveJSON defaultOptions ''ConfigFile)

readConfigFile :: FilePath -> IO (Either AppError B.ByteString)
readConfigFile fn = fmap (first ConfigFileReadError) $ try $ B.readFile fn

getConfig :: FilePath -> IO (Either AppError ConfigFile)
getConfig fn = do
    print $ "Reading config from: " ++ fn
    confFileContentOrErr <- readConfigFile fn
    let decodedConfFileOrErr = confFileContentOrErr >>= first JSONDecodeError . eitherDecode 
    return decodedConfFileOrErr

writeConfig :: FilePath -> ConfigFile -> IO (Maybe AppError)
writeConfig fn confFile = 
    fmap (either (Just . ConfigFileWriteError) (const Nothing)) $ try $ encodeFile fn confFile

saveConfig :: FilePath -> EnvConfig -> AppMonad ()
saveConfig fn conf = do
    liftIO $ print $ "Writing config to: " ++ fn
    maybeErr <- liftIO $ writeConfig fn (ConfigFile $ fmap getAuthToken (configAuthToken conf))
    forM_ maybeErr throwError
