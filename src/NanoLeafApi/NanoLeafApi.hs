{-# LANGUAGE OverloadedStrings #-}

module NanoLeafApi.NanoLeafApi
    ( addUser
    , getAllPanelInfo
    , getOnOffState )
    where

--aeson

import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import Data.Aeson (object, (.=), encode)
import Data.Text (Text, unpack)
import qualified Types as T
--import Control.Lens.Setter ((%~),(.~))
import Control.Lens.Getter ((^.))
import NanoLeafApi.Types

--TODO: finish up the AppMonad in AppMonad.hs before anything else
--
--
    --also need serialization of the allPanelInfo responsed for instance

--TODO: probably want to reuse the a same manager instead of creating a new one all the time?
    --Should perhaps be passed as state similar to a db connection
    --Do not add more functionality until those things are in place

addUser :: T.NanoLeaf -> IO ()
addUser nf = do
  manager <- newManager defaultManagerSettings

  let endpoint = "/api/v1/new"
  let hostname = unpack $ T.getHostName $ nf ^. T.hostname
  let rPort = show $ T.getPort $ nf ^. T.port
  let url = "http://" ++ hostname ++ ":" ++ rPort ++ endpoint
  initialRequest <- parseRequest url
  let request = initialRequest { method = "POST" }

  response <- httpLbs request manager
  putStrLn $ "The status code was: " ++ show (statusCode $ responseStatus response)
  print $ responseBody response


getAllPanelInfo :: AuthToken -> T.NanoLeaf -> IO ()
getAllPanelInfo authToken nf = do
  manager <- newManager defaultManagerSettings

  let endpoint = "/api/v1/" ++ (unpack $ getAuthToken authToken) ++ "/"
  let hostname = unpack $ T.getHostName $ nf ^. T.hostname
  let rPort = show $ T.getPort $ nf ^. T.port
  let url = "http://" ++ hostname ++ ":" ++ rPort ++ endpoint
  initialRequest <- parseRequest url
  let request = initialRequest { method = "GET" }

  response <- httpLbs request manager
  putStrLn $ "The status code was: " ++ show (statusCode $ responseStatus response)
  print $ responseBody response

getOnOffState :: AuthToken -> T.NanoLeaf -> IO ()
getOnOffState authToken nf = do
  manager <- newManager defaultManagerSettings
  let endpoint = "/api/v1/" ++ (unpack $ getAuthToken authToken) ++ "/state/on"

  let hostname = unpack $ T.getHostName $ nf ^. T.hostname
  let rPort = show $ T.getPort $ nf ^. T.port
  let url = "http://" ++ hostname ++ ":" ++ rPort ++ endpoint
  initialRequest <- parseRequest url
  let request = initialRequest { method = "GET" }

  response <- httpLbs request manager
  putStrLn $ "The status code was: " ++ show (statusCode $ responseStatus response)
  print $ responseBody response

--getAuthToken ()
--getPanelInfo
--
--possible helpers
--createEndPointUrl
