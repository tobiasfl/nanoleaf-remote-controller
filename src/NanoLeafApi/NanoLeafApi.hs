{-# LANGUAGE OverloadedStrings #-}

module NanoLeafApi.NanoLeafApi
    ( addUser
    , getAllPanelInfo
    , getOnOffState 
    , setOnOffState 
    , initManager 
    , getBrightnessState
    , setBrightnessState)
    where

import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import Data.Aeson (object, (.=), encode)
import Data.Text (unpack)
import qualified Types as T
--import Control.Lens.Setter ((%~),(.~))
import Control.Lens.Getter ((^.))
import NanoLeafApi.Types
import AppMonad (AppMonad, AppError (RequestWithoutAuthToken), liftIO, EnvConfig (EnvConfig, configAuthToken, connManager))
import Control.Monad.Reader (reader, ask)
import Control.Monad.Except (throwError)

--TODO: also need serialization of the allPanelInfo responsed for instance

initManager :: IO Manager
initManager = newManager defaultManagerSettings

createGetRequest :: T.NanoLeaf -> String -> AppMonad Request
createGetRequest nf endPoint = do
    maybeAuthTok <- reader configAuthToken
    authTok <- maybe (throwError RequestWithoutAuthToken) return maybeAuthTok

    let url = createUrl nf endPoint (Just authTok)
    initialRequest <- liftIO $ parseRequest url
    return $ initialRequest { method = "GET" }

createUrl :: T.NanoLeaf -> String -> Maybe AuthToken -> String
createUrl nf endPoint maybeAuthTok = "http://" ++ hostname ++ ":" ++ rPort ++ endPointWithAuth
    where endPointWithAuth = "/api/v1/" ++ unpack (maybe "" getAuthToken maybeAuthTok) ++ endPoint
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
getAllPanelInfo nf = do
  request <- createGetRequest nf "/"
  manager <- reader connManager

  response <- liftIO $ httpLbs request manager
  --TODO: close connection by using withResponse instead
  liftIO $ putStrLn $ "The status code was: " ++ show (statusCode $ responseStatus response)
  liftIO $ print $ responseBody response

getOnOffState :: T.NanoLeaf -> AppMonad ()
getOnOffState nf = do
  request <- createGetRequest nf "/state/on"
  manager <- reader connManager

  response <- liftIO $ httpLbs request manager
  --TODO: close connection by using withResponse instead
  liftIO $ putStrLn $ "The status code was: " ++ show (statusCode $ responseStatus response)
  liftIO $ print $ responseBody response

setOnOffState :: T.NanoLeaf -> Bool -> AppMonad ()
setOnOffState nf newOnOffState = do
    (EnvConfig maybeAuthTok manager) <- ask
    authTok <- maybe (throwError RequestWithoutAuthToken) return maybeAuthTok
    let url = createUrl nf "/state" (Just authTok)
    initialRequest <- liftIO $ parseRequest url
    let requestObject = object ["on" .= object ["value" .= newOnOffState]]
    let request = initialRequest { method = "PUT", requestBody = RequestBodyLBS $ encode requestObject }
    response <- liftIO $ httpNoBody request manager
    --TODO: close connection by using withResponse instead
    liftIO $ putStrLn $ "The status code was: " ++ show (statusCode $ responseStatus response)

getBrightnessState :: T.NanoLeaf -> AppMonad ()
getBrightnessState nf = do
  request <- createGetRequest nf "/state/brightness"
  manager <- reader connManager

  response <- liftIO $ httpLbs request manager
  --TODO: close connection by using withResponse instead
  liftIO $ putStrLn $ "The status code was: " ++ show (statusCode $ responseStatus response)
  liftIO $ print $ responseBody response

setBrightnessState :: T.NanoLeaf -> Int -> AppMonad ()
setBrightnessState nf newBrightness = do
    (EnvConfig maybeAuthTok manager) <- ask
    authTok <- maybe (throwError RequestWithoutAuthToken) return maybeAuthTok

    let url = createUrl nf "/state" (Just authTok)
    initialRequest <- liftIO $ parseRequest url

    let requestObject = object ["brightness" .= object ["value" .= newBrightness]]
    let request = initialRequest { method = "PUT", requestBody = RequestBodyLBS $ encode requestObject }

    response <- liftIO $ httpNoBody request manager
    --TODO: close connection by using withResponse instead
    liftIO $ putStrLn $ "The status code was: " ++ show (statusCode $ responseStatus response)
