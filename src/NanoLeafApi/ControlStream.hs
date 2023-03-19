
module NanoLeafApi.ControlStream (
    continuousVolumeMeter,
    continuousWaves,
    continuousAllEffects)
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
import Control.Monad
import Data.ByteString.Builder (toLazyByteString, word16BE)
import qualified NanoLeafApi.Alsa.Alsa as ALSA
import NanoLeafApi.Effects (volumeMeterEffect, waveEffect, PanelUpdate(..), Effect, layerEffectUpdates, lightAllEffect, EffectUpdate)
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, tryTakeMVar, MVar)
import Data.List.Extra (headDef)

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
createStreamingMsgFromMap :: EffectUpdate -> BS.ByteString
createStreamingMsgFromMap idsToColors = header <> body
        where header = BS.pack $ map fromIntegral [0, Prelude.length idsToColors] 
              body :: ByteString
              body = BS.concat $ map (\(pid, PanelUpdate r g b tt) -> encodeWord16 pid <> BS.pack (map fromIntegral [r, g, b, 0]) <> encodeWord16 tt) idsToColors
              encodeWord16 :: Int -> BS.ByteString
              encodeWord16 = BSL.toStrict . toLazyByteString . word16BE . fromIntegral 

doEffect :: ControlStreamHandle -> Effect -> IO ()
doEffect handle effect = do
    mapM_ doUpdate effect
        where doUpdate panelUpdateMap = do 
                sendByteString handle $ createStreamingMsgFromMap panelUpdateMap
                threadDelay (updateRateMs * 1000)

--TODO: needs a way to stop as well
--TODO: check time since last message and make it wait until 100ms
doEffectsWhileMeasuring :: ControlStreamHandle -> MVar Int -> [PanelId] -> [Effect] -> IO ()
doEffectsWhileMeasuring handle volMVar ids effects = do
    vol <- takeMVar volMVar
    let wave = if vol > 3000 then waveEffect ids else []
    let vMeter = volumeMeterEffect ids vol
    let flashEffect = if vol > 8000 then lightAllEffect ids else []
    let withNewEffects = vMeter:wave:flashEffect:effects
    let emptyEffectUpdate = replicate 15 (0, PanelUpdate 0 0 0 0)
    let layeredEffect = layerEffectUpdates $ map (headDef emptyEffectUpdate) withNewEffects
    sendByteString handle (createStreamingMsgFromMap layeredEffect)
    threadDelay (updateRateMs * 1000)
    let filteredEffects = filter (not . null) $ map (Prelude.drop 1) withNewEffects
    doEffectsWhileMeasuring handle volMVar ids filteredEffects

continuousAllEffects :: NanoLeaf -> [PanelId] -> IO () 
continuousAllEffects nl ids = do
    handle <- createSocket nl
    volMVar <- newEmptyMVar
    threadId <- forkIO (ALSA.volumeMeter volMVar)
    doEffectsWhileMeasuring handle volMVar ids []
    closeSocket handle

continuousWaves :: NanoLeaf -> [PanelId] -> IO ()
continuousWaves nl ids = do
    handle <- createSocket nl
    volMVar <- newEmptyMVar
    threadId <- forkIO (ALSA.volumeMeter volMVar)
    forever $ do
        volume <- takeMVar volMVar
        when (volume > 2000) (doEffect handle (waveEffect ids))
    closeSocket handle

continuousVolumeMeter :: NanoLeaf -> [PanelId] -> IO ()
continuousVolumeMeter nl ids = do
    handle <- createSocket nl
    mVar <- newEmptyMVar
    threadId <- forkIO (ALSA.volumeMeter mVar)
    forever $ do
        volume <- takeMVar mVar
        let panelColorsMap = head $ volumeMeterEffect ids volume
        sendByteString handle (createStreamingMsgFromMap panelColorsMap)
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

updateRateMs :: Int
updateRateMs = 100

