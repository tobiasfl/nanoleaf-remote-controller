{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Mdns
    ( findNanoleafs )
    where

import Shelly (shelly, run, Sh)
import Data.Text (Text, pack, unpack)
import Text.Read (readEither)
import Types
import Text.Parsec as P

findNanoleafs :: IO [NanoLeaf]
findNanoleafs = do
    stdOutput <- avahiBrowseAndResolve
    either (error . show) pure $ P.parse avahiBrowsParser "" stdOutput

avahiBrowseAndResolve :: IO Text
avahiBrowseAndResolve = shelly avahiBrowseAndResolveRun
    
avahiBrowseAndResolveRun :: Sh Text
avahiBrowseAndResolveRun = run "avahi-browse" ["_nanoleafapi._tcp", "--resolve", "-t"]

--TODO: use ParsecT and combine with Application monad instead
avahiBrowsParser :: P.Parsec Text () [NanoLeaf]
avahiBrowsParser = do
    P.skipMany $ P.char '+' >> P.manyTill P.anyChar (P.char '\n')
    P.many nanoLeafParser

nanoLeafParser :: P.Parsec Text () NanoLeaf
nanoLeafParser = do
    P.char '=' >> P.manyTill P.anyChar (P.char '\n')
    hostName <- HostName <$> bracketContentParser
    addr <- Address <$> bracketContentParser
    port <- bracketContentParser >>= (either error (pure . Port) . readEither . unpack)
    mkNanoLeaf port hostName addr <$> nanoLeafInfoParser

bracketContentParser :: P.Parsec Text () Text
bracketContentParser =  
    P.manyTill P.anyChar (P.char '[') >> pack <$> P.manyTill P.anyChar (P.char ']')

nanoLeafInfoParser :: Parsec Text () NanoLeafInfo
nanoLeafInfoParser = do
    P.manyTill P.anyChar (P.char '[')
    firmwareVer <-FirmwareVersion <$> fieldParser
    modelName <- ModelName <$> fieldParser
    devId <- DeviceId <$> fieldParser
    P.manyTill P.anyChar (P.char '\n')
    return $ mkNanoLeafInfo modelName firmwareVer devId
        where fieldParser = 
                pack <$> (P.manyTill P.anyChar (P.char '=') >> P.manyTill P.anyChar (P.char '\"'))
