{-# LANGUAGE TemplateHaskell #-}

module Types 
    ( NanoLeafInfo 
    , NanoLeaf
    , hostname
    , address
    , port
    , mkNanoLeafInfo
    , mkNanoLeaf
    , Port (..)
    , Address (..)
    , HostName (..)
    , ModelName (..)
    , FirmwareVersion (..)
    , DeviceId (..)
    ) where

import GHC.Word (Word16)
import Data.Text (Text)
import Control.Lens.TH (makeLenses)

newtype ModelName = ModelName { getModelName :: Text } 
    deriving (Show, Eq)

newtype FirmwareVersion = FirmwareVersion { getFirmwareVersion :: Text } 
    deriving (Show, Eq)

newtype DeviceId = DeviceId { getDeviceId :: Text } 
    deriving (Show, Eq)

newtype Port = Port { getPort :: Word16 } 
    deriving (Show, Eq)

newtype HostName = HostName { getHostName :: Text } 
    deriving (Show, Eq)

newtype Address = Address { getAddress :: Text } 
    deriving (Show, Eq)

data NanoLeafInfo = NanoLeafInfo
    { _modelName :: ModelName 
    , _firmwareVers :: FirmwareVersion
    , _id :: DeviceId --Randomly generated, changes if a user resets the device
    }
    deriving (Eq, Show)

makeLenses ''NanoLeafInfo

mkNanoLeafInfo
    :: ModelName
    -> FirmwareVersion
    -> DeviceId
    -> NanoLeafInfo
mkNanoLeafInfo = NanoLeafInfo

data NanoLeaf = NanoLeaf
    { _port :: Port 
    , _hostname :: HostName
    , _address :: Address
    , _nanoLeafInfo :: NanoLeafInfo
    }
    deriving (Eq, Show)

makeLenses ''NanoLeaf

mkNanoLeaf 
    :: Port 
    -> HostName
    -> Address
    -> NanoLeafInfo
    -> NanoLeaf
mkNanoLeaf = NanoLeaf 
