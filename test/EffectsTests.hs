{-# LANGUAGE TupleSections #-}

module EffectsTests (runTests) 
    where

import Test.QuickCheck
import qualified Data.Set as S (fromList)

import Data.List.Extra (lastDef)

import Control.Monad (liftM4)
import NanoLeafApi.Effects
import Data.Functor ((<&>))
import NanoLeafApi.Types (StateValue(..), PanelLayout (..), Layout(..), PanelPositionData (PanelPositionData), layout, panelId)

genPanelLayout :: Gen PanelLayout
genPanelLayout = do
  numPanels <- choose (1, 100)
  let panelIds = [0..numPanels-1]
  panelPositions <- mapM (\pid -> (do
      x <- choose (0, 500)
      y <- choose (0, 500)
      orientation <- choose (0, 360)
      shapeType <- elements [8, 12]
      return $ PanelPositionData pid x y orientation shapeType)) panelIds
  globalOrientation <- choose (0, 360)
  return $ PanelLayout (StateValue globalOrientation 360 0) (Layout numPanels 0 panelPositions)

genVolume :: Gen Volume 
genVolume = choose (0, 10000)

genEffectDir :: Gen EffectDir
genEffectDir = elements [L, R, U]

genPanelUpdate :: Gen PanelUpdate
genPanelUpdate = do
    let genRGB = choose (0, 255)
    liftM4 PanelUpdate genRGB genRGB genRGB (choose (0, 100))

genEffectUpdate :: Gen EffectUpdate
genEffectUpdate = do
    numPanels <- choose (1, 100)
    let panelIds = [0..numPanels-1]
    mapM (\pid -> genPanelUpdate <&> (pid,)) panelIds   

prop_volumeMeterEffect_effectLengthAlwaysOne :: Property
prop_volumeMeterEffect_effectLengthAlwaysOne = forAll genPanelLayout $ \pl ->
  forAll genVolume $ \vol ->
    let effect = volumeMeterEffect pl vol
     in length effect === 1

prop_volumeMeterEffect_alwaysLightsAtLeastOnePanel :: Property
prop_volumeMeterEffect_alwaysLightsAtLeastOnePanel = forAll genPanelLayout $ \pl ->
  forAll genVolume $ \vol ->
    let effect = volumeMeterEffect pl vol
        litPanels = concatMap (filter ((darkenPanelUpdate /=) . snd)) effect 
     in not (null litPanels)

prop_volumeMeterEffect_updatesAllPanels :: Property
prop_volumeMeterEffect_updatesAllPanels = forAll genPanelLayout $ \pl ->
  forAll genVolume $ \vol ->
    let effect = volumeMeterEffect pl vol
        panelsToBeUpdated = map fst $ concat effect
        expectedPanelsToBeUpdated = concat $ filter (/=[0]) $ map ((:[]) . panelId) $ positionData $ layout pl
     in S.fromList panelsToBeUpdated === S.fromList expectedPanelsToBeUpdated

prop_waveEffect_effectLengthMatchesNumberOfPanelsPlusOne :: Property
prop_waveEffect_effectLengthMatchesNumberOfPanelsPlusOne = forAll genPanelLayout $ \pl ->
  forAll genEffectDir $ \dir ->
    let effect = waveEffect dir pl
        expectedEffectLength = length (filter (/= 0) (map panelId $ positionData $ layout pl)) + 1
     in length effect === expectedEffectLength

prop_waveEffect_oneDifferentPanelLightsUpEveryUpdate :: Property
prop_waveEffect_oneDifferentPanelLightsUpEveryUpdate = forAll genPanelLayout $ \pl ->
  forAll genEffectDir $ \dir ->
    let effect = waveEffect dir pl
        panelsLitUp = map (map fst . filter (\(_, pu) -> pu /= darkenPanelUpdate)) effect
        expectedPanelsLitUp = filter (/=[0]) $ map ((:[]) . panelId) $ positionData $ layout pl
     in S.fromList (concat panelsLitUp) === S.fromList (concat expectedPanelsLitUp)

prop_waveEffect_oneLigthEveryUpdateExceptLast :: Property
prop_waveEffect_oneLigthEveryUpdateExceptLast = forAll genPanelLayout $ \pl ->
  forAll genEffectDir $ \dir ->
    let effect = waveEffect dir pl
        panelsLitUp = map (map fst . filter (\(_, pu) -> pu /= darkenPanelUpdate)) effect
     in conjoin (map ((1===) . length) (drop 1 $ reverse  panelsLitUp)) 

prop_waveEffect_lastUpdateAlwaysDark :: Property
prop_waveEffect_lastUpdateAlwaysDark = forAll genPanelLayout $ \pl ->
  forAll genEffectDir $ \dir ->
    let effect = waveEffect dir pl
        lastUpdate = lastDef [] effect
        expectedNumberOfPanelsInLast = length $ filter (/= 0) (map panelId $ positionData $ layout pl)
     in expectedNumberOfPanelsInLast === length lastUpdate .&&. all ((darkenPanelUpdate ==) . snd) lastUpdate

--prop_layerEffectUpdates_layersByListOrder :: Property
--prop_layerEffectUpdates_layersByListOrder = 
--  forAll (listOf genEffectUpdate) $ \updates ->
--    let layeredUpdate = layerEffectUpdates updates
--        --TODO: might be a bit complicated to check this

--prop_layerEffectUpdates_worksOnEffectUpdatesWithDifferentLengths :: Property
--prop_layerEffectUpdates_worksOnEffectUpdatesWithDifferentLengths = 



-- run the tests
runTests :: IO ()
runTests = do
  quickCheck prop_waveEffect_effectLengthMatchesNumberOfPanelsPlusOne
  quickCheck prop_waveEffect_oneDifferentPanelLightsUpEveryUpdate
  quickCheck prop_waveEffect_oneLigthEveryUpdateExceptLast
  quickCheck prop_waveEffect_lastUpdateAlwaysDark
  quickCheck prop_volumeMeterEffect_updatesAllPanels 
  quickCheck prop_volumeMeterEffect_effectLengthAlwaysOne
  quickCheck prop_volumeMeterEffect_alwaysLightsAtLeastOnePanel
