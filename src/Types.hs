{-# LANGUAGE TemplateHaskell   #-}

module Types 
    ( NanoLeafInfo
    , NanoLeaf
    , mkNanoLeafInfo
    , mkNanoLeaf
    , Port
    ) where

import GHC.Word (Word16)
import Data.Text (Text)
import Control.Lens.TH (makeLenses)
--import Control.Lens.Setter ((%~),(.~))

type ModelName = Text

type FirmwareVersion = Text

type DeviceId = Text

type Port = Word16

type HostName = Text

type Address = Text

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


