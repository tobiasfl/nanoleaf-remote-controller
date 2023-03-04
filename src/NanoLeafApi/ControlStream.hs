
module NanoLeafApi.ControlStream (
    sendMessage
    , sendMessageForever)
    where

import Network.Socket
import Network.Socket.ByteString
import qualified Data.ByteString as BS (ByteString, length, pack, concat)
import qualified Data.ByteString.Lazy as BSL (toStrict, ByteString)
import Data.ByteString.UTF8 as BSU
import Data.Text (unpack)
import Types (NanoLeaf, getHostName, hostname)
import Control.Lens.Getter ((^.))
import NanoLeafApi.Types (PanelId)
import Control.Concurrent (threadDelay)
import Control.Monad
import Data.List ((\\))
import Data.ByteString.Builder (toLazyByteString, word16BE)
import qualified NanoLeafApi.Alsa.Alsa as ALSA
import qualified Data.Map as M
--TODO: make more of the imports qualified

data ControlStreamHandle = ControlStreamHandle 
    { sock :: Socket 
    , addr :: SockAddr
    }

--TODO: exception handling

defaultUdpPort :: String
defaultUdpPort = "60222"

createSocket :: NanoLeaf -> IO ControlStreamHandle
createSocket nf = do
    print "Creating UDP socket"
    addrInfos <- getAddrInfo Nothing (Just (unpack $ getHostName $ nf ^. hostname)) (Just defaultUdpPort)
    let serverAddr = head addrInfos

    cshSock <- socket (addrFamily serverAddr) Datagram defaultProtocol
    return $ ControlStreamHandle cshSock (addrAddress serverAddr)

--TODO: Consider converting whole thing to use lazy ByteString instead
--TODO: Also need a version of this function that can get a spec of different colors for each panelId(a map from id to colour)
createStreamingMsg :: [PanelId] -> (Int, Int, Int, Int) -> BS.ByteString
createStreamingMsg ids (r, g, b, transitionTime100ms) = header <> body
        where header = BS.pack $ map fromIntegral [0, Prelude.length ids] 
              body :: ByteString
              body = BS.concat $ map (\pid -> encodeWord16 pid <> BS.pack (map fromIntegral [r, g, b, 0]) <> encodeWord16 transitionTime100ms) ids
              encodeWord16 :: Int -> BS.ByteString
              encodeWord16 = BSL.toStrict . toLazyByteString . word16BE . fromIntegral 

createStreamingMsgFromMap :: M.Map PanelId (Int, Int, Int, Int) -> BS.ByteString
createStreamingMsgFromMap idsToColors = header <> body
        where header = BS.pack $ map fromIntegral [0, M.size idsToColors] 
              body :: ByteString
              body = BS.concat $ map (\(pid, (r, g, b, tt)) -> encodeWord16 pid <> BS.pack (map fromIntegral [r, g, b, 0]) <> encodeWord16 tt) $ M.toList idsToColors
              encodeWord16 :: Int -> BS.ByteString
              encodeWord16 = BSL.toStrict . toLazyByteString . word16BE . fromIntegral 



sendMessage :: NanoLeaf -> [PanelId] -> IO ()
sendMessage nf ids = do
    handle <- createSocket nf
    let msg = createStreamingMsg ids (255, 255, 255, 1)
    sendByteString handle msg
    closeSocket handle

greenMsg :: [PanelId] -> BS.ByteString
greenMsg ids = createStreamingMsg ids (0, 255, 0, 1)

blueMsg :: [PanelId] -> BS.ByteString
blueMsg ids = createStreamingMsg ids (0, 0, 255, 1)

redMsg :: [PanelId] -> BS.ByteString
redMsg ids = createStreamingMsg ids (255, 0, 0, 1)

volumeToColor :: Int -> (Int, Int, Int, Int) 
volumeToColor vol = (r, g, b, 1)
    where r = vol `div` 255
          g = vol `div` 255
          b = vol `div` 255

--pretending that 17000 is max TODO: find real max
volumeToPanelCount :: Int -> [PanelId] -> Int
volumeToPanelCount volume panelIds = (volume `div` toDivBy) + 1
    where maxVolume = 17000
          toDivBy = maxVolume `div` Prelude.length panelIds

volumeToPanelIds :: Int -> [PanelId] -> [PanelId]
volumeToPanelIds volume panelIds = Prelude.take (volumeToPanelCount volume panelIds) panelIds

--TODO: add timer to make sure you only send msg every 100ms
sendMessageForever :: NanoLeaf -> [PanelId] -> IO ()
sendMessageForever nl ids = do
    handle <- createSocket nl
    ALSA.volumeMeter (\volume -> do
        let withExtraVolume = volume * 3
        let panelsToLightUp = volumeToPanelIds withExtraVolume ids
        let panelsToDarken = ids \\ panelsToLightUp
        let panelColorsMap = M.fromList $ zip panelsToLightUp (repeat (255, 0, 0, 1)) ++ zip panelsToDarken (repeat (0, 0, 0, 1))
        sendByteString handle (createStreamingMsgFromMap panelColorsMap))
    closeSocket handle   

sendByteString :: ControlStreamHandle -> BS.ByteString -> IO ()
sendByteString csHandle msg = do
    print "Sending UDP message"
    sentBytes <- sendTo (sock csHandle) msg (addr csHandle) 
    print $ "sent: " ++ show sentBytes ++ "/" ++ show (BS.length msg) ++ " bytes"
 
closeSocket :: ControlStreamHandle -> IO ()
closeSocket csHandle = do 
    print "Closing UDP socket"
    close (sock csHandle)



