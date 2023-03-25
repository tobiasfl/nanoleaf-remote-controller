{-# LANGUAGE TupleSections #-}

module NanoLeafApi.Effects (
    volumeMeterEffect
    , waveEffect
    , PanelUpdate (..)
    , EffectUpdate
    , Effect
    , layerEffectUpdates
    , lightAllEffect
    , EffectDir (..))
    where

import NanoLeafApi.Types (PanelId, Layout, PanelLayout)
import NanoLeafApi.PanelLayout (panelIdsFromRight, panelIdsFromLeft, panelIdsFromBottom)
import Data.List ((\\), sortOn)

type Volume = Int

--TODO: could have a field to describe the type of the Effect (e.g. Wave, meeter, splash etc.)
type Effect = [EffectUpdate]

--data Effect

data EffectDir = L | R | U
    deriving (Show, Eq)

type EffectUpdate = [(PanelId, PanelUpdate)]

data PanelUpdate = PanelUpdate {
    red :: Int,
    green :: Int,
    blue :: Int,
    transitionTime :: Int --in 100ms (so 1 == 100ms)
}

---TODO: move to outside this lib and pass it as arg to funcs
--pretending that 17000 is max TODO: 
--Could use moving average (or 95th percentile) to know what is max
maxVolume :: Volume
maxVolume = 10000

darkenPanelUpdate :: PanelUpdate
darkenPanelUpdate = PanelUpdate 0 0 0 1

greenPanelUpdate :: PanelUpdate
greenPanelUpdate = PanelUpdate 0 255 0 1

redPanelUpdate :: PanelUpdate
redPanelUpdate = PanelUpdate 255 0 0 1

volumeToPanelCount :: Volume -> [PanelId] -> Int
volumeToPanelCount volume panelIds = (volume `div` toDivBy) + minimumPanelsToLight
    where toDivBy = maxVolume `div` length panelIds
          minimumPanelsToLight = 1

volumeToPanelIds :: Int -> [PanelId] -> [PanelId]
volumeToPanelIds volume panelIds = take (volumeToPanelCount volume panelIds) panelIds

volumeMeterEffect :: PanelLayout -> Volume -> Effect
volumeMeterEffect pl volume = [zip panelsToLightUp (repeat redPanelUpdate) ++ zip panelsToDarken (repeat darkenPanelUpdate)]
    where withExtraVolume = volume * 3
          ids = panelIdsFromBottom pl
          panelsToLightUp = volumeToPanelIds withExtraVolume ids
          panelsToDarken = ids \\ panelsToLightUp
         
waveEffect :: EffectDir -> PanelLayout -> Effect
waveEffect dir layout = map (`ligthOnePanelEffectUpdate` ids) ids ++ darkenAllPanelsEffect ids
    where ids = case dir of L -> panelIdsFromRight layout
                            R -> panelIdsFromLeft layout
                            U -> panelIdsFromBottom layout

volumeToPanelUpdate :: Volume -> PanelUpdate
volumeToPanelUpdate vol = PanelUpdate brightness brightness brightness 1
    where brightness = vol `div` toDivBy
          toDivBy = maxVolume `div` maxBrightness
          maxBrightness = 255

lightAllEffect :: PanelLayout -> Volume -> Effect
lightAllEffect pl vol = [map (, pu) ids] 
    where pu = volumeToPanelUpdate vol
          ids = panelIdsFromBottom pl 

ligthOnePanelEffectUpdate :: PanelId -> [PanelId] -> [(PanelId, PanelUpdate)]
ligthOnePanelEffectUpdate idToLight = map (\x -> (x, if idToLight == x then greenPanelUpdate else darkenPanelUpdate))

darkenAllPanelsEffect :: [PanelId] -> Effect
darkenAllPanelsEffect ids = [zip ids (repeat darkenPanelUpdate)]

--TODO: assumes effectUpdates are same length and all including the same ids
layerEffectUpdates :: [EffectUpdate] -> EffectUpdate
layerEffectUpdates [] = []
layerEffectUpdates [x] = x
layerEffectUpdates (low:up:xs) = layerEffectUpdates (layered:xs)
    where 
        layered = zipWith layerPanelUpdates (sortOn fst low) (sortOn fst up)
        layerPanelUpdates :: (PanelId, PanelUpdate) -> (PanelId, PanelUpdate) -> (PanelId, PanelUpdate)
        layerPanelUpdates lower (_, PanelUpdate 0 0 0 _) = lower
        layerPanelUpdates (_, PanelUpdate 0 0 0 _) upper = upper
        layerPanelUpdates _ upper = upper

--TODO: effect that depends one some moving average (and then lights some or all panels depending on ranges of volume) e.g. 5, 25, 50(avg), 75, 95 percentiles or something
