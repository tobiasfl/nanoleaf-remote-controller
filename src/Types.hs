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
    , Command (..)
    , Request (..)
    , AuthToken 
    , mkAuthToken
    , getAuthToken 
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

newtype AuthToken = AuthToken
    { getAuthToken :: Text }
    deriving (Show, Eq)

mkAuthToken :: Text -> AuthToken
mkAuthToken = AuthToken


data Request = Request
  { getCommand          :: Command
  , getInitialAuthToken :: Maybe AuthToken 
  } 
  deriving (Show)

--TODO: could perhaps also pass in some nanoleaf ID like e.g. IP or hostname to some commands
--that the user may aquire by first running ListNanoLeafs

data Command = GetAllPanelInfo
             | GetNewAuthToken
             | OnOffState
             | TurnOff
             | TurnOn
             | ShowBrightness
             | SetBrightness Int
             | ListEffects
             | GetSelectedEffect
             | SetSelectedEffect String
             | ListNanoLeafs  --TODO: use MDNS to list possible nanoleafs
             deriving (Show, Eq)


