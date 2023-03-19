{-# LANGUAGE TupleSections #-}

module NanoLeafApi.Effects (
    volumeMeterEffect
    , waveEffect
    , PanelUpdate (..)
    , EffectUpdate
    , Effect
    , layerEffectUpdates
    , lightAllEffect)
    where

import NanoLeafApi.Types (PanelId)
import Data.List ((\\), sortOn)

type Volume = Int

--TODO: could have a field to describe the type of the Effect (e.g. Wave, meeter, splash etc.)
type Effect = [EffectUpdate]

type EffectUpdate = [(PanelId, PanelUpdate)]

data PanelUpdate = PanelUpdate {
    red :: Int,
    green :: Int,
    blue :: Int,
    transitionTime :: Int --in 100ms (so 1 == 100ms)
}

---TODO: move to outside this lib and pass it as arg to funcs
--pretending that 17000 is max TODO: find real max
maxVolume :: Volume
maxVolume = 17000

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

--assumes panelIds are sorted from left to right according to their actual layout
volumeMeterEffect :: [PanelId] -> Volume -> Effect
volumeMeterEffect ids volume = [zip panelsToLightUp (repeat redPanelUpdate) ++ zip panelsToDarken (repeat darkenPanelUpdate)]    
    where withExtraVolume = volume * 3
          panelsToLightUp = volumeToPanelIds withExtraVolume ids
          panelsToDarken = ids \\ panelsToLightUp
         
--assumes panelIds are sorted from left to right according to their actual layout
waveEffect :: [PanelId] -> Effect
waveEffect ids = map (`ligthOnePanelEffect` ids) ids

lightAllEffect :: [PanelId] -> Effect
lightAllEffect ids = [map (, PanelUpdate 255 255 255 1) ids]

ligthOnePanelEffect ::  PanelId -> [PanelId] -> [(PanelId, PanelUpdate)]
ligthOnePanelEffect idToLight = map (\x -> (x, if idToLight == x then greenPanelUpdate else darkenPanelUpdate))

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

