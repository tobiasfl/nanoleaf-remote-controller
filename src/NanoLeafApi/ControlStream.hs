
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
import Data.ByteString.Builder (toLazyByteString, word16BE)

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
createStreamingMsg :: [PanelId] -> (Int, Int, Int, Int) -> BS.ByteString
createStreamingMsg ids (r, g, b, transitionTime100ms) = header <> body
        where header = BS.pack $ map fromIntegral [0, Prelude.length ids] 
              body :: ByteString
              body = BS.concat $ map (\pid -> encodeWord16 pid <> BS.pack (map fromIntegral [r, g, b, 0]) <> encodeWord16 transitionTime100ms) ids
              encodeWord16 :: Int -> BS.ByteString
              encodeWord16 = BSL.toStrict . toLazyByteString . word16BE . fromIntegral 

sendMessage :: NanoLeaf -> [PanelId] -> IO ()
sendMessage nf ids = do
    handle <- createSocket nf
    let msg = createStreamingMsg ids (255, 255, 255, 1)
    sendByteString handle msg
    closeSocket handle

sendMessageForever :: NanoLeaf -> [PanelId] -> IO ()
sendMessageForever nl ids = do
    handle <- createSocket nl
    let greenMsg = createStreamingMsg ids (0, 255, 0, 1)
    let blueMsg = createStreamingMsg ids (0, 0, 255, 1)
    let redMsg = createStreamingMsg ids (255, 0, 0, 1)
    forever (do
        threadDelay 1000000
        sendByteString handle greenMsg
        threadDelay 1000000
        sendByteString handle blueMsg
        threadDelay 1000000
        sendByteString handle redMsg
        )
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



