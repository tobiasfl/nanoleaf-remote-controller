module NanoLeafApi.Effects (
    volumeMeterEffect
    , PanelUpdate (..))
    where

import NanoLeafApi.Types (PanelId)
import Data.List ((\\))

type Volume = Int

data PanelUpdate = PanelUpdate {
    red :: Int,
    green :: Int,
    blue :: Int,
    transitionTime :: Int --in 100ms (so 1 == 100ms)
}

--pretending that 17000 is max TODO: find real max
volumeToPanelCount :: Int -> [PanelId] -> Int
volumeToPanelCount volume panelIds = (volume `div` toDivBy) + minimumPanelsToLight
    where maxVolume = 17000
          toDivBy = maxVolume `div` Prelude.length panelIds
          minimumPanelsToLight = 1

volumeToPanelIds :: Int -> [PanelId] -> [PanelId]
volumeToPanelIds volume panelIds = Prelude.take (volumeToPanelCount volume panelIds) panelIds

volumeMeterEffect :: [PanelId] -> Volume -> [(PanelId, PanelUpdate)]
volumeMeterEffect ids volume = zip panelsToLightUp (repeat (PanelUpdate 255 0 0 1)) ++ zip panelsToDarken (repeat (PanelUpdate 0 0 0 1))
    where withExtraVolume = volume * 3
          panelsToLightUp = volumeToPanelIds withExtraVolume ids
          panelsToDarken = ids \\ panelsToLightUp
          
