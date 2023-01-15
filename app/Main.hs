module Main (main) where

import Lib
import Mdns

main :: IO ()
main = do
    nanoLeafs <- findNanoleafs
    mapM_ print nanoLeafs
    someFunc
