module Main (main) where

import Lib
import Mdns
import NanoLeafApi

main :: IO ()
main = do
    nanoLeafs <- findNanoleafs
    --requestAuthToken (head nanoLeafs)
