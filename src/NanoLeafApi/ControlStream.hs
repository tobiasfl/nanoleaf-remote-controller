
module NanoLeafApi.ControlStream (
    testSendMessage)
    where

import Network.Socket
import Network.Socket.ByteString
import qualified Data.ByteString as BS (ByteString, length)
import Data.ByteString.UTF8 as BSU
import Data.Text (unpack)
import Types (NanoLeaf, getHostName, hostname)
import Control.Lens.Getter ((^.))

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

    sock <- socket (addrFamily serverAddr) Datagram defaultProtocol
    return $ ControlStreamHandle sock (addrAddress serverAddr)
 
testMsg :: String
testMsg = 
    "0 2  ,---> nPanels\n1 18014 255 255 255 0 0 5  ,---> Set panel color\n2 45036 255 255 255 0 0 5  ,---> Set panel color"

testSendMessage :: NanoLeaf -> IO ()
testSendMessage nf = do
    handle <- createSocket nf
    print $ "message:\n" ++ testMsg
    let msg = BSU.fromString testMsg
    sendByteString handle msg
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



