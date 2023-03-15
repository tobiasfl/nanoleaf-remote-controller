
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
import NanoLeafApi.Effects (volumeMeterEffect, PanelUpdate(..))

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

createStreamingMsgFromMap :: [(PanelId, PanelUpdate)] -> BS.ByteString
createStreamingMsgFromMap idsToColors = header <> body
        where header = BS.pack $ map fromIntegral [0, Prelude.length idsToColors] 
              body :: ByteString
              body = BS.concat $ map (\(pid, PanelUpdate r g b tt) -> encodeWord16 pid <> BS.pack (map fromIntegral [r, g, b, 0]) <> encodeWord16 tt) idsToColors
              encodeWord16 :: Int -> BS.ByteString
              encodeWord16 = BSL.toStrict . toLazyByteString . word16BE . fromIntegral 

sendMessage :: NanoLeaf -> [PanelId] -> IO ()
sendMessage nf ids = do
    handle <- createSocket nf
    let msg = createStreamingMsg ids (255, 255, 255, 1)
    sendByteString handle msg
    closeSocket handle

--TODO: add timer to make sure you only send msg every 100ms
sendMessageForever :: NanoLeaf -> [PanelId] -> IO ()
sendMessageForever nl ids = do
    handle <- createSocket nl
    ALSA.volumeMeter (\volume -> do
        let panelColorsMap = volumeMeterEffect ids volume
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



