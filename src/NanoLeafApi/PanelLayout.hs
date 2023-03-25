module NanoLeafApi.PanelLayout (
    panelIdsFromLeft
    , panelIdsFromRight
    , panelIdsFromBottom)
    where

import NanoLeafApi.Types (StateValue(value), PanelPositionData(..), PanelLayout, Layout(positionData), PanelId, PanelLayout(layout, globalOrientation))
import Data.List (sortOn)

triangleShapeSideLength :: Int
triangleShapeSideLength = 134

panelIdsFromLeft :: PanelLayout -> [PanelId]
panelIdsFromLeft pl = filter (0/=) $ map panelId $ sortOn x $ rotatePositionData (-degrees) $ positionData $ layout pl
    where degrees = value $ globalOrientation pl

panelIdsFromRight :: PanelLayout -> [PanelId]
panelIdsFromRight = reverse . panelIdsFromLeft

panelIdsFromBottom :: PanelLayout -> [PanelId]
panelIdsFromBottom pl = filter (0/=) $ map panelId $ sortOn y $ rotatePositionData (-degrees) $ positionData $ layout pl
    where degrees = value $ globalOrientation pl

panelIdsFromTop :: PanelLayout -> [PanelId]
panelIdsFromTop = reverse . panelIdsFromBottom

rotatePositionData :: Int -> [PanelPositionData] -> [PanelPositionData]
rotatePositionData degrees = map rotate
  where rotate (PanelPositionData id x y o t) =
          let x' = fromIntegral x
              y' = fromIntegral y
              radians = fromIntegral degrees * pi / 180.0
           in PanelPositionData id (round (x' * cos radians - y' * sin radians)) (round (x' * sin radians + y' * cos radians)) o t

