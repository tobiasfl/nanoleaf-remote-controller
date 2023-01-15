{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Mdns
    ( findNanoleafs )
    where

import Shelly (shelly, run, Sh)
import Data.Text (Text, pack, unpack)
import Text.Read (readMaybe, readEither)
import Types (NanoLeaf, mkNanoLeaf, NanoLeafInfo, mkNanoLeafInfo, Port)
import Text.Parsec as P
import Data.Either (fromRight)

--Currently just using avahi-browse

--TODO: parser interface for output from avahiBrowse and use in findNanoleafs

findNanoleafs :: IO [NanoLeaf]
findNanoleafs = do
    stdOutput <- avahiBrowseAndResolve
    nanoLeafs <- either (error . show) pure $ P.parse nanoLeafParser "" stdOutput
    return nanoLeafs


avahiBrowseAndResolve :: IO Text
avahiBrowseAndResolve = shelly avahiBrowseAndResolveRun
    
avahiBrowseAndResolveRun :: Sh Text
avahiBrowseAndResolveRun = run "avahi-browse" ["_nanoleafapi._tcp", "--resolve", "-t"]


--TODO: use ParsecT and combine with Application monad instead
nanoLeafParser :: P.Parsec Text () [NanoLeaf]
nanoLeafParser = do
    P.skipMany $ P.manyTill P.anyChar (P.char '\n')
    P.char '=' >> P.manyTill P.anyChar (P.char '\n')
    hostName <- bracketContentParser
    addr <- bracketContentParser
    port <- bracketContentParser >>= (either error pure . readEither . unpack)
    nanoeLeafInfo <- nanoLeafInfoParser
    return [mkNanoLeaf port hostName addr nanoeLeafInfo]

bracketContentParser :: P.Parsec Text () Text
bracketContentParser = --TODO: this one may be prettier with P.between
    P.manyTill P.anyChar (P.char '[') >> pack <$> P.manyTill P.anyChar (P.char ']')

--TODO: finish this
nanoLeafInfoParser :: Parsec Text () NanoLeafInfo
nanoLeafInfoParser = do
    P.manyTill P.anyChar (P.char '[') 
    firmwareVer <- P.manyTill P.anyChar (P.char '=') >> P.manyTill P.anyChar (P.char '\"')
    modelName <- P.manyTill P.anyChar (P.char '=') >> P.manyTill P.anyChar (P.char '\"') 
    devId <-  P.manyTill P.anyChar (P.char '=') >> P.manyTill P.anyChar (P.char '\"') 
    return $ mkNanoLeafInfo """"""

     

fromTxt :: [Text] -> NanoLeafInfo
fromTxt = undefined
