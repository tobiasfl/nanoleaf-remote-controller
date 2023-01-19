module Main (main) where

import Mdns
import NanoLeafApi
import System.Environment (getArgs)
import Data.Maybe (listToMaybe)
import NanoLeafApi.Types (mkAuthToken)
import Data.Text (pack)

main :: IO ()
main = do
    arg <- listToMaybe <$> getArgs
    nanoLeafs <- findNanoleafs
    --requestAuthToken (head nanoLeafs)
    --maybe (pure ()) (`getAllPanelInfo` head nanoLeafs) arg
    maybe (pure ()) ((`getOnOffState` head nanoLeafs) . mkAuthToken . pack) arg
