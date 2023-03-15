{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module NanoLeafApi.Types
    (AllPanelInfo (panelLayout)
    , PanelId
    , PanelPositionData (panelId)
    , PanelLayout (layout)
    , Layout (positionData)
    )
    where

import Data.Aeson (FromJSON, parseJSON, withObject, (.:))
import Data.Aeson.TH
import Data.Text (Text)

type PanelId = Int

data PanelPositionData = PanelPositionData  {
    panelId :: PanelId,
    x :: Int, --X coordinate of the centroid of the panel
    y :: Int, --Y coordinate of the centroid of the panel
    o :: Int, --Orientation of the panel
    shapeType :: Int --
    } 
    deriving (Show)

$(deriveFromJSON defaultOptions ''PanelPositionData)

data Layout = Layout {
    numPanels :: Int,
    sideLength :: Int, --Not in use, always 0(must be inferred from type)
    positionData :: [PanelPositionData]
    } 
    deriving (Show)

$(deriveFromJSON defaultOptions ''Layout)

data StateValue = 
    StateValue {
        value :: Int,
        max :: Int,
        min :: Int
    } deriving (Show)

$(deriveFromJSON defaultOptions ''StateValue)

newtype OnStateValue = OnStateValue Bool
    deriving (Show)

instance FromJSON OnStateValue where
    parseJSON = withObject "value" $ \v -> OnStateValue <$> v .: "value"

--"on\":{\"value\":true}

data State = State {
    brightness :: StateValue,
    colorMode :: Text,
    ct :: StateValue,
    hue :: StateValue,
    on :: OnStateValue,--create custom 
    sat :: StateValue
    } deriving (Show)

$(deriveFromJSON defaultOptions ''State)

data EffectsList = EffectsList {
    effectsList :: [Text],
    select :: Text
    } deriving (Show)

$(deriveFromJSON defaultOptions ''EffectsList)

data PanelLayout = PanelLayout {
    globalOrientation :: StateValue,
    layout :: Layout
   } deriving (Show)

$(deriveFromJSON defaultOptions ''PanelLayout)

--TODO: some of the fields of the data types should perhaps be Maybe's

data AllPanelInfo = AllPanelInfo {
    name :: Text,
    serialNo :: Text,
    manufacturer :: Text,
    firmwareVersion :: Text,
    hardwareVersion :: Text,
    model :: Text,
    --discovery,
    effects :: EffectsList,
    --firmwareUpgrade
    panelLayout :: PanelLayout,
    --schedules
    state :: State
    } deriving (Show)

$(deriveFromJSON defaultOptions ''AllPanelInfo)
--TODO: make a test with this

--"{\"name\":\"Shapes 2B2B\",\"serialNo\":\"S21280HM465\",\"manufacturer\":\"Nanoleaf\",\"firmwareVersion\":\"7.1.1\",\"hardwareVersion\":\"1.4-0\",\"model\":\"NL42\",\"discovery\":{},\"effects\":{\"effectsList\":[\"Acidic Dishwasher\",\"Bass Beat drop\",\"Be Productive\",\"Beach Waves\",\"Beatdrop\",\"Bedtime\",\"Blaze\",\"Classic Visualizer\",\"Cocoa Beach\",\"Cotton Candy\",\"Date Night\",\"Energize\",\"Fall leafs\",\"Fantasia\",\"Hip Hop\",\"Hot Sauce\",\"Infected M\195\188shrooms\",\"Japanese Streets\",\"Jungle\",\"Lightning\",\"Lightscape\",\"Lover Boy\",\"Morning Sky\",\"Northern Lights\",\"Outer Space Full Color\",\"Pop Rocks\",\"Prism\",\"RGBeat the Cold\",\"Retro\",\"Sparrows\",\"Starlight\",\"Sundown\",\"Try this before you skip it\",\"Vibrant Sunrise\",\"Waking Rest\",\"Waterfall\"],\"select\":\"*ExtControl*\"},\"firmwareUpgrade\":{},\"panelLayout\":{\"globalOrientation\":{\"value\":119,\"max\":360,\"min\":0},\"layout\":{\"numPanels\":16,\"sideLength\":134,\"positionData\":[{\"panelId\":47502,\"x\":536,\"y\":5,\"o\":0,\"shapeType\":8},{\"panelId\":18014,\"x\":469,\"y\":43,\"o\":180,\"shapeType\":8},{\"panelId\":45036,\"x\":402,\"y\":5,\"o\":240,\"shapeType\":8},{\"panelId\":8909,\"x\":335,\"y\":43,\"o\":180,\"shapeType\":8},{\"panelId\":1190,\"x\":268,\"y\":5,\"o\":120,\"shapeType\":8},{\"panelId\":51133,\"x\":402,\"y\":121,\"o\":240,\"shapeType\":8},{\"panelId\":64200,\"x\":335,\"y\":159,\"o\":180,\"shapeType\":8},{\"panelId\":13301,\"x\":268,\"y\":121,\"o\":0,\"shapeType\":8},{\"panelId\":41414,\"x\":201,\"y\":159,\"o\":60,\"shapeType\":8},{\"panelId\":54968,\"x\":134,\"y\":121,\"o\":240,\"shapeType\":8},{\"panelId\":56362,\"x\":268,\"y\":237,\"o\":120,\"shapeType\":8},{\"panelId\":4499,\"x\":201,\"y\":275,\"o\":60,\"shapeType\":8},{\"panelId\":3574,\"x\":134,\"y\":237,\"o\":240,\"shapeType\":8},{\"panelId\":5486,\"x\":67,\"y\":275,\"o\":60,\"shapeType\":8},{\"panelId\":39372,\"x\":0,\"y\":237,\"o\":120,\"shapeType\":8},{\"panelId\":0,\"x\":595,\"y\":0,\"o\":300,\"shapeType\":12}]}},\"qkihnokomhartlnp\":{},\"schedules\":{},\"state\":{\"brightness\":{\"value\":100,\"max\":100,\"min\":0},\"colorMode\":\"effect\",\"ct\":{\"value\":2700,\"max\":6500,\"min\":1200},\"hue\":{\"value\":0,\"max\":360,\"min\":0},\"on\":{\"value\":true},\"sat\":{\"value\":0,\"max\":100,\"min\":0}}}"
