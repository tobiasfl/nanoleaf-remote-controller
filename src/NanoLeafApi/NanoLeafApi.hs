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
    , setSelectedEffect
    , startStreaming
    )
    where

import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import Data.Aeson (object, (.=), encode, Value, eitherDecode, FromJSON)
import Data.Text (unpack)
import qualified Types as T
import Control.Lens.Getter ((^.))
import NanoLeafApi.Types
import AppMonad (AppMonad, AppError (RequestWithoutAuthToken, JSONDecodeError), liftIO, EnvConfig (EnvConfig, configAuthToken, connectionManager))
import Control.Monad.Reader (reader, ask)
import Control.Monad.Except (throwError)
import NanoLeafApi.ControlStream (continuousAllEffects)

--TODO: wrap some of the calls in try and mapping to an appropriate AppError

initManager :: IO Manager
initManager = newManager defaultManagerSettings

doGetRequestJSON :: FromJSON a => T.NanoLeaf -> String -> AppMonad a 
doGetRequestJSON nl endPoint = do
  request <- createGetRequest nl endPoint
  manager <- reader connectionManager

  liftIO $ putStrLn $ "Sending GET request: " ++ show request
  response <- liftIO $ httpLbs request manager
  --TODO: close connection by using withResponse instead
  liftIO $ putStrLn $ "The status code was: " ++ show (statusCode $ responseStatus response)
  either (throwError . JSONDecodeError) return $ eitherDecode $ responseBody response

doGetRequest :: T.NanoLeaf -> String -> AppMonad ()
doGetRequest nl endPoint = do
  request <- createGetRequest nl endPoint
  manager <- reader connectionManager

  liftIO $ putStrLn $ "Sending GET request: " ++ show request
  response <- liftIO $ httpLbs request manager
  --TODO: close connection by using withResponse instead
  liftIO $ putStrLn $ "The status code was: " ++ show (statusCode $ responseStatus response)
  liftIO $ print $ responseBody response

createGetRequest :: T.NanoLeaf -> String -> AppMonad Request
createGetRequest nl endPoint = do
    maybeAuthTok <- reader configAuthToken
    authTok <- maybe (throwError RequestWithoutAuthToken) return maybeAuthTok

    let url = createUrl nl endPoint (Just authTok)
    initialRequest <- liftIO $ parseRequest url
    return $ initialRequest { method = "GET" }

doPutRequest :: T.NanoLeaf -> Value -> String -> AppMonad ()
doPutRequest nl requestObject endPoint = do
    (EnvConfig maybeAuthTok manager) <- ask
    authTok <- maybe (throwError RequestWithoutAuthToken) return maybeAuthTok

    let url = createUrl nl endPoint (Just authTok)
    initialRequest <- liftIO $ parseRequest url
    let request = initialRequest { method = "PUT", requestBody = RequestBodyLBS $ encode requestObject }
    liftIO $ putStrLn $ "Sending PUT request: " ++ show request
    response <- liftIO $ httpLbs request manager
    liftIO $ putStrLn $ "The status code was: " ++ show (statusCode $ responseStatus response)
    liftIO $ print $ responseBody response

createUrl :: T.NanoLeaf -> String -> Maybe T.AuthToken -> String
createUrl nl endPoint maybeAuthTok = "http://" ++ hostname ++ ":" ++ rPort ++ endPointWithAuth
    where endPointWithAuth = "/api/v1/" ++ unpack (maybe "" T.getAuthToken maybeAuthTok) ++ endPoint
          hostname = unpack $ T.getHostName $ nl ^. T.hostname
          rPort = show $ T.getPort $ nl ^. T.port

addUser :: T.NanoLeaf -> IO ()
addUser nl = do
  manager <- newManager defaultManagerSettings
  let url = createUrl nl "/new" Nothing
  initialRequest <- parseRequest url
  let request = initialRequest { method = "POST" }
  response <- httpLbs request manager
  putStrLn $ "The status code was: " ++ show (statusCode $ responseStatus response)
  print $ responseBody response

getAllPanelInfo :: T.NanoLeaf -> AppMonad AllPanelInfo
getAllPanelInfo nl = doGetRequestJSON nl "/"

getOnOffState :: T.NanoLeaf -> AppMonad ()
getOnOffState nl = doGetRequest nl  "/state/on"

setOnOffState :: T.NanoLeaf -> Bool -> AppMonad ()
setOnOffState nl newOnOffState = do
    let requestObject = object ["on" .= object ["value" .= newOnOffState]]
    doPutRequest nl requestObject "/state"

getBrightnessState :: T.NanoLeaf -> AppMonad ()
getBrightnessState nl = doGetRequest nl "/state/brightness"

setBrightnessState :: T.NanoLeaf -> Int -> AppMonad ()
setBrightnessState nl newBrightness = do
    let requestObject = object ["brightness" .= object ["value" .= newBrightness]]
    doPutRequest nl requestObject "/state"

getEffects :: T.NanoLeaf -> AppMonad ()
getEffects nl = doGetRequest nl "/effects/effectsList"

getSelectedEffect :: T.NanoLeaf -> AppMonad ()
getSelectedEffect nl = doGetRequest nl "/effects/select"

setSelectedEffect :: T.NanoLeaf -> String -> AppMonad ()
setSelectedEffect nl effect = do
    --TODO: handle resource not found(which means the effect does not exist)
    let requestObject = object ["select" .= effect ]
    doPutRequest nl requestObject "/effects"

--Can be verified by checking that nanoleaf selected mode is
    --"\"*ExtControl*\""
requestControlStream :: T.NanoLeaf -> AppMonad () 
requestControlStream nl = do
    let requestObject = object [ "write" .= object [ 
            "command" .= ("display" :: String), 
            "extControlVersion" .= ("v2" :: String), 
            "animType" .= ("extControl" :: String)] ]
    doPutRequest nl requestObject "/effects"
 
--TODO: maybe take a selected program as agr, then choose effects based on that
startStreaming :: T.NanoLeaf -> [String] -> AppMonad ()
startStreaming nl effects = do
    layout <- getPanelLayout nl
    requestControlStream nl
    --TODO: check last message was successful and that selected effect is
    --"\"*ExtControl*\""
    liftIO $ continuousAllEffects nl layout effects

getPanelLayout :: T.NanoLeaf -> AppMonad PanelLayout
getPanelLayout nl = do
    allPanelInfo <- getAllPanelInfo nl
    return $ panelLayout allPanelInfo 
