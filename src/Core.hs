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
import Config (getConfig)

--TODO: set up testing
--TODO: file for authtoken

runApp :: IO ()
runApp = do
    (Request cmd auth) <- execParser opts
    connManager <- initManager
    configFromFile <- getConfig "/tmp/testconfig.json"--TODO: use this!
    let envConf = EnvConfig auth connManager
    let initialState = AppState Nothing
    nanoLeafsOrErr <- runAppMonad (handleCommand cmd) envConf initialState
    print nanoLeafsOrErr
        where
            opts = info (CL.requestParser <**> helper)
              ( fullDesc
              <> progDesc "Control Nanoleafs from the command line"
              <> header "NanoLeaf Controller")

--displayAppError :: AppError -> String TODO

--prepareAppReqs :: IO EnvConfig TODO: will look for config(json) file with authToken

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
                 StartNanoLeafExtCtrl -> requestControlStream (head nanoLeafs)
                 _ -> liftIO $ putStrLn $ show cmd ++ " is not implemented!")
   
--getNewAuthToken :: AppMonad AuthToken TODO
