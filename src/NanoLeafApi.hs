{-# LANGUAGE OverloadedStrings #-}

module NanoLeafApi
    ( requestAuthToken )
    where

--aeson

import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import Data.Aeson (object, (.=), encode)
import Data.Text (Text, unpack)
import qualified Types as T
--import Control.Lens.Setter ((%~),(.~))
import Control.Lens.Getter ((^.))

newtype AuthToken = AuthToken
    { getAuthToken :: Text }
    deriving (Show, Eq)

--TODO: make an app monad and error type and stuff

requestAuthToken :: T.NanoLeaf -> IO ()
requestAuthToken nf = do
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


getAllPanelInfo :: T.NanoLeaf -> IO ()
getAllPanelInfo = undefined

addUser :: T.NanoLeaf -> IO ()
addUser = undefined

--getAuthToken ()
--addUser
--getPanelInfo
--
--possible helpers
--createEndPointUrl
