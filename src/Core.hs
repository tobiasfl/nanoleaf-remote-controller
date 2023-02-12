module Core
    ( runApp
    ) where

import Mdns
import AppMonad (liftIO, AppError (NanoLeafsNotFound), AppMonad, runAppMonad, AppState(..), EnvConfig(..))
import NanoLeafApi 
import Control.Monad.Except (throwError)
import Control.Monad (when)
import Options.Applicative
import qualified CommandLine as CL
import Types
import Config (getConfig, saveConfig, authenticationToken)

--TODO: set up testing

runApp :: IO ()
runApp = do
    (Request cmd authTokOverride) <- execParser opts
    envConf  <- liftIO $ prepareAppReqs authTokOverride
    let initialState = AppState Nothing
    nanoLeafsOrErr <- runAppMonad (handleCommand cmd) envConf initialState
    print nanoLeafsOrErr
        where
            opts = info (CL.requestParser <**> helper)
              ( fullDesc
              <> progDesc "Control Nanoleafs from the command line"
              <> header "NanoLeaf Controller")

--displayAppError :: AppError -> String TODO

prepareAppReqs :: Maybe AuthToken -> IO EnvConfig
prepareAppReqs authTokOverride = do
    connManager <- initManager
    configFromFileOrErr <- getConfig "config.json"
    --TODO: handle thrown errors
    let authTokenToUse = authTokOverride <|> either (const Nothing) (fmap mkAuthToken . authenticationToken) configFromFileOrErr
    return $ EnvConfig authTokenToUse connManager

handleCommand :: Command -> AppMonad ()
handleCommand cmd = do
    nanoLeafs <- findNanoleafs 
    when (null nanoLeafs) (throwError NanoLeafsNotFound)    
    --TODO: handle when no token is configured
    (case cmd of GetAllPanelInfo -> getAllPanelInfo (head nanoLeafs) >>= liftIO . print
                 OnOffState -> getOnOffState $ head nanoLeafs
                 TurnOff -> setOnOffState (head nanoLeafs) False
                 TurnOn -> setOnOffState (head nanoLeafs) True
                 ShowBrightness -> getBrightnessState (head nanoLeafs)
                 SetBrightness level -> setBrightnessState (head nanoLeafs) level
                 ListEffects -> getEffects (head nanoLeafs)
                 GetSelectedEffect -> getSelectedEffect (head nanoLeafs)
                 SetSelectedEffect effect -> setSelectedEffect (head nanoLeafs) effect
                 StartNanoLeafExtCtrl -> startStreaming (head nanoLeafs)
                 _ -> liftIO $ putStrLn $ show cmd ++ " is not implemented!")
   
--getNewAuthToken :: AppMonad AuthToken TODO
