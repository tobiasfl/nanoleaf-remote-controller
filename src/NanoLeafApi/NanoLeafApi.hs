{-# LANGUAGE OverloadedStrings #-}

module NanoLeafApi.NanoLeafApi
    ( addUser
    , getAllPanelInfo
    , getOnOffState 
    , setOnOffState 
    , initManager 
    , getBrightnessState
    , setBrightnessState
    , getEffects
    , getSelectedEffect
    , setSelectedEffect)
    where

import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import Data.Aeson (object, (.=), encode, Value)
import Data.Text (unpack)
import qualified Types as T
--import Control.Lens.Setter ((%~),(.~))
import Control.Lens.Getter ((^.))
import NanoLeafApi.Types
import AppMonad (AppMonad, AppError (RequestWithoutAuthToken), liftIO, EnvConfig (EnvConfig, configAuthToken, connectionManager))
import Control.Monad.Reader (reader, ask)
import Control.Monad.Except (throwError)

--TODO: also need serialization of the allPanelInfo responsed for instance

initManager :: IO Manager
initManager = newManager defaultManagerSettings

--TODO: getRequestWithEndpointJSON :: JSON e => T.NanoLeaf -> String -> AppMonad e

doGetRequest :: T.NanoLeaf -> String -> AppMonad ()
doGetRequest nf endPoint = do
  request <- createGetRequest nf endPoint
  manager <- reader connectionManager

  response <- liftIO $ httpLbs request manager
  --TODO: close connection by using withResponse instead
  liftIO $ putStrLn $ "The status code was: " ++ show (statusCode $ responseStatus response)
  liftIO $ print $ responseBody response

createGetRequest :: T.NanoLeaf -> String -> AppMonad Request
createGetRequest nf endPoint = do
    maybeAuthTok <- reader configAuthToken
    authTok <- maybe (throwError RequestWithoutAuthToken) return maybeAuthTok

    let url = createUrl nf endPoint (Just authTok)
    initialRequest <- liftIO $ parseRequest url
    return $ initialRequest { method = "GET" }

doPutRequest :: T.NanoLeaf -> Value -> String -> AppMonad ()
doPutRequest nf requestObject endPoint = do
    (EnvConfig maybeAuthTok manager) <- ask
    authTok <- maybe (throwError RequestWithoutAuthToken) return maybeAuthTok

    let url = createUrl nf endPoint (Just authTok)
    initialRequest <- liftIO $ parseRequest url
    let request = initialRequest { method = "PUT", requestBody = RequestBodyLBS $ encode requestObject }
    response <- liftIO $ httpNoBody request manager
    --TODO: close connection by using withResponse instead
    liftIO $ putStrLn $ "The status code was: " ++ show (statusCode $ responseStatus response)

createUrl :: T.NanoLeaf -> String -> Maybe T.AuthToken -> String
createUrl nf endPoint maybeAuthTok = "http://" ++ hostname ++ ":" ++ rPort ++ endPointWithAuth
    where endPointWithAuth = "/api/v1/" ++ unpack (maybe "" T.getAuthToken maybeAuthTok) ++ endPoint
          hostname = unpack $ T.getHostName $ nf ^. T.hostname
          rPort = show $ T.getPort $ nf ^. T.port

addUser :: T.NanoLeaf -> IO ()
addUser nf = do
  manager <- newManager defaultManagerSettings
  let url = createUrl nf "/new" Nothing
  initialRequest <- parseRequest url
  let request = initialRequest { method = "POST" }
  response <- httpLbs request manager
  putStrLn $ "The status code was: " ++ show (statusCode $ responseStatus response)
  print $ responseBody response

--TODO: consider wrapping some of the calls in try and mapping to an appropriate AppError
getAllPanelInfo :: T.NanoLeaf -> AppMonad ()
getAllPanelInfo nf = doGetRequest nf "/"

getOnOffState :: T.NanoLeaf -> AppMonad ()
getOnOffState nf = doGetRequest nf  "/state/on"

setOnOffState :: T.NanoLeaf -> Bool -> AppMonad ()
setOnOffState nf newOnOffState = do
    let requestObject = object ["on" .= object ["value" .= newOnOffState]]
    doPutRequest nf requestObject "/state"

getBrightnessState :: T.NanoLeaf -> AppMonad ()
getBrightnessState nf = doGetRequest nf "/state/brightness"

setBrightnessState :: T.NanoLeaf -> Int -> AppMonad ()
setBrightnessState nf newBrightness = do
    let requestObject = object ["brightness" .= object ["value" .= newBrightness]]
    doPutRequest nf requestObject "/state"

getEffects :: T.NanoLeaf -> AppMonad ()
getEffects nf = doGetRequest nf "/effects/effectsList"

getSelectedEffect :: T.NanoLeaf -> AppMonad ()
getSelectedEffect nf = doGetRequest nf "/effects/select"

setSelectedEffect :: T.NanoLeaf -> String -> AppMonad ()
setSelectedEffect nf effect = do
    --TODO: handle resource not found
    let requestObject = object ["select" .= effect ]
    doPutRequest nf requestObject "/effects"

--requestControlStream :: T.NanoLeaf -> AppMonad () TODO: get info for udp socket

