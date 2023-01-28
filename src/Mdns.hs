{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Mdns
    ( findNanoleafs )
    where

import Shelly (shelly, run, Sh)
import Data.Text (Text, pack, unpack)
import Text.Read (readEither)
import qualified Types as T
import qualified Text.Parsec as P
import AppMonad (AppMonad, AppError (AvahiBrowseParseError, AvahiBrowseCommandException), liftIO)
import Control.Monad.Error.Class (throwError)
import Control.Exception (try, SomeException )

findNanoleafs :: AppMonad [T.NanoLeaf]
findNanoleafs = do
    stdOutput <- avahiBrowseAndResolve
    either (throwError . AvahiBrowseParseError) return $ P.parse avahiBrowsParser "" stdOutput

avahiBrowseAndResolve :: AppMonad Text
avahiBrowseAndResolve = do
    errOrText <- liftIO (try $ shelly avahiBrowseAndResolveRun :: IO (Either SomeException Text))
    case errOrText of Left e -> throwError $ AvahiBrowseCommandException $ show e 
                      Right t -> return t

avahiBrowseAndResolveRun :: Sh Text
avahiBrowseAndResolveRun = run "avahi-browse" ["_nanoleafapi._tcp", "--resolve", "-t"]

--TODO: This one should succeed with an empty list even if there are no nanoleafs found (or just throw NanoLeafsNotFound if it fails)
avahiBrowsParser :: P.Parsec Text () [T.NanoLeaf]
avahiBrowsParser = do
    P.skipMany $ P.char '+' >> P.manyTill P.anyChar (P.char '\n')
    P.many nanoLeafParser

nanoLeafParser :: P.Parsec Text () T.NanoLeaf
nanoLeafParser = do
    P.char '=' >> P.manyTill P.anyChar (P.char '\n')
    hostName <- T.HostName <$> bracketContentParser
    addr <- T.Address <$> bracketContentParser
    port <- bracketContentParser >>= (either error (pure . T.Port) . readEither . unpack)
    T.mkNanoLeaf port hostName addr <$> nanoLeafInfoParser

bracketContentParser :: P.Parsec Text () Text
bracketContentParser =  
    P.manyTill P.anyChar (P.char '[') >> pack <$> P.manyTill P.anyChar (P.char ']')

nanoLeafInfoParser :: P.Parsec Text () T.NanoLeafInfo
nanoLeafInfoParser = do
    P.manyTill P.anyChar (P.char '[')
    firmwareVer <-T.FirmwareVersion <$> fieldParser
    modelName <- T.ModelName <$> fieldParser
    devId <- T.DeviceId <$> fieldParser
    P.manyTill P.anyChar (P.char '\n')
    return $ T.mkNanoLeafInfo modelName firmwareVer devId
        where fieldParser = 
                pack <$> (P.manyTill P.anyChar (P.char '=') >> P.manyTill P.anyChar (P.char '\"'))
