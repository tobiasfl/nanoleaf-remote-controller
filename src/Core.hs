module Core
    ( runApp
    ) where

import Mdns
import AppMonad (liftIO, AppError (NanoLeafsNotFound), AppMonad, runAppMonad, AppState(..), EnvConfig(..))
import NanoLeafApi 
import Control.Monad.Except (throwError)
import Control.Monad (when)
import Control.Monad.Reader (asks)
import Options.Applicative
import qualified CommandLine as CL
import Data.Maybe (isNothing)
import Types
import Config (getConfig, authenticationToken, ConfigFile)

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

displayAppError :: AppError -> String
displayAppError = undefined

prepareAppReqs :: Maybe AuthToken -> IO EnvConfig
prepareAppReqs authTokOverride = do
    connManager <- initManager
    configFromFileOrErr <- getConfig "config.json"
    --TODO: handle thrown errors, (either eat them, print an explaining string, or crash program)
    let authTokenToUse = resolveAuthToken authTokOverride configFromFileOrErr
    return $ EnvConfig authTokenToUse connManager

resolveAuthToken :: Maybe AuthToken -> Either AppError ConfigFile -> Maybe AuthToken 
resolveAuthToken authTokOverride configFromFileOrErr = do 
    authTokOverride <|> either (const Nothing) (fmap mkAuthToken . authenticationToken) configFromFileOrErr

handleCommand :: Command -> AppMonad ()
handleCommand cmd = do
    nanoLeafs <- findNanoleafs 
    when (null nanoLeafs) (throwError NanoLeafsNotFound)    
    maybeAuthTok <- asks configAuthToken
    (case cmd of GetAllPanelInfo -> getAllPanelInfo (head nanoLeafs) >>= liftIO . print
                 OnOffState -> getOnOffState $ head nanoLeafs
                 TurnOff -> setOnOffState (head nanoLeafs) False
                 TurnOn -> setOnOffState (head nanoLeafs) True
                 ShowBrightness -> getBrightnessState (head nanoLeafs)
                 SetBrightness level -> setBrightnessState (head nanoLeafs) level
                 ListEffects -> getEffects (head nanoLeafs)
                 GetSelectedEffect -> getSelectedEffect (head nanoLeafs)
                 SetSelectedEffect effect -> setSelectedEffect (head nanoLeafs) effect
                 StartNanoLeafExtCtrl streamEffects -> handleStartStreamingCommand (head nanoLeafs) streamEffects
                 _ -> liftIO $ putStrLn $ show cmd ++ " is not implemented!")
  
handleMissingAuthToken :: AppMonad AuthToken
handleMissingAuthToken = undefined

handleStartStreamingCommand :: NanoLeaf -> [String] -> AppMonad ()
handleStartStreamingCommand nl effects = do
    startStreaming nl effects

getNewAuthToken :: AppMonad AuthToken
getNewAuthToken = undefined
