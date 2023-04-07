module EffectsTests (runTests) 
    where

import Test.QuickCheck
import qualified Data.Set as S (fromList)

-- import the module containing the functions to test
import NanoLeafApi.Effects

import NanoLeafApi.Types (StateValue(..), PanelLayout (..), Layout(..), PanelPositionData (PanelPositionData), layout, panelId)


-- define some generators for the input values to the functions
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

genVolume :: Gen Int --TODO: maybe use Volume directly instead
genVolume = choose (0, 10000)

genEffectDir :: Gen EffectDir
genEffectDir = elements [L, R, U]

prop_volumeMeterEffect_panelsLitUp :: Property
prop_volumeMeterEffect_panelsLitUp = forAll genPanelLayout $ \pl ->
  forAll genVolume $ \vol ->
    let effect = volumeMeterEffect pl vol
        panelsLitUp = length $ filter (\(_, pu) -> pu == PanelUpdate 255 0 0 1) $ concat effect
        expectedPanelsLitUp = undefined
     in panelsLitUp === expectedPanelsLitUp

prop_volumeMeterEffect_alwaysOneUpdate :: Property
prop_volumeMeterEffect_alwaysOneUpdate = forAll genPanelLayout $ \pl ->
  forAll genVolume $ \vol ->
    let effect = volumeMeterEffect pl vol
     in length effect === 1

prop_waveEffect_updateLengthMatchesNumberOfPanels :: Property
prop_waveEffect_updateLengthMatchesNumberOfPanels = forAll genPanelLayout $ \pl ->
  forAll genEffectDir $ \dir ->
    let effect = waveEffect dir pl
        panelsLitUp = length $ filter (\(_, pu) -> pu == PanelUpdate 0 255 0 1) $ concat effect
        expectedPanelsLitUp = length $ concat $ take 1 effect
     in panelsLitUp === expectedPanelsLitUp

prop_waveEffect_oneDifferentPanelLightsUpEveryUpdate :: Property
prop_waveEffect_oneDifferentPanelLightsUpEveryUpdate = forAll genPanelLayout $ \pl ->
  forAll genEffectDir $ \dir ->
    let effect = waveEffect dir pl
        panelsLitUp = map (map fst . filter (\(_, pu) -> pu /= PanelUpdate 0 0 0 1)) effect
        expectedPanelsLitUp = filter (/=[0]) $ map ((:[]) . panelId) $ positionData $ layout pl
     in S.fromList (concat panelsLitUp) === S.fromList (concat expectedPanelsLitUp)

prop_waveEffect_oneLigthEveryUpdateExceptLast :: Property
prop_waveEffect_oneLigthEveryUpdateExceptLast = forAll genPanelLayout $ \pl ->
  forAll genEffectDir $ \dir ->
    let effect = waveEffect dir pl
        panelsLitUp = map (map fst . filter (\(_, pu) -> pu /= PanelUpdate 0 0 0 1)) effect
     in conjoin (map ((1===) . length) (drop 1 $ reverse  panelsLitUp)) 



--prop_waveEffect_lastUpdateAlwaysDark :: Property
--prop_waveEffect_lastUpdateAlwaysDark = forAll genPanelLayout $ \pl ->
--  forAll genEffectDir $ \dir ->
--    let effect = waveEffect dir pl
--     in 



-- run the tests
runTests :: IO ()
runTests = do
  quickCheck prop_waveEffect_updateLengthMatchesNumberOfPanels
  quickCheck prop_waveEffect_oneDifferentPanelLightsUpEveryUpdate
  quickCheck prop_waveEffect_oneLigthEveryUpdateExceptLast
  quickCheck prop_volumeMeterEffect_alwaysOneUpdate
