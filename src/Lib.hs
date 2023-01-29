module Lib
    ( runApp
    ) where

import Mdns
import AppMonad (AppError (NanoLeafsNotFound), AppMonad, runAppMonad, AppState(..), EnvConfig(..))
import System.Environment (getArgs)
import Data.Maybe (listToMaybe)
import NanoLeafApi 
import Data.Text (pack)
import Control.Monad.Except (throwError)
import Control.Monad (when)


runApp :: IO ()
runApp = do
    arg <- listToMaybe <$> getArgs
    connManager <- initManager
    let envConf = EnvConfig (fmap (mkAuthToken . pack) arg) connManager
    let initialState = AppState Nothing
    nanoLeafsOrErr <- runAppMonad application envConf initialState
    print nanoLeafsOrErr

--displayAppError :: AppError -> String TODO

application :: AppMonad ()
application = do
    nanoLeafs <- findNanoleafs 
    when (null nanoLeafs) (throwError NanoLeafsNotFound)
    --addUser (head nanoLeafs) TODO: if there is no configured token
    --getAllPanelInfo $ head nanoLeafs
    --getOnOffState $ head nanoLeafs
    setOnOffState (head nanoLeafs) True
    --getBrightnessState $ head nanoLeafs
    --setBrightnessState (head nanoLeafs) 100

