
module NanoLeafApi.ControlStream (
    continuousAllEffects
    )
    where

import Network.Socket
import Network.Socket.ByteString
import qualified Data.ByteString as BS (ByteString, length, pack, concat)
import qualified Data.ByteString.Lazy as BSL (toStrict, ByteString)
import Data.Text (unpack)
import Types (NanoLeaf, getHostName, hostname)
import Control.Lens.Getter ((^.))
import NanoLeafApi.Types (PanelId, Layout, PanelLayout)
import Control.Monad
import Data.ByteString.Builder (toLazyByteString, word16BE)
import qualified NanoLeafApi.Alsa.Alsa as ALSA
import NanoLeafApi.Effects 
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, tryTakeMVar, MVar)
import Data.List.Extra (headDef)
import NanoLeafApi.PanelLayout (panelIdsFromLeft, panelIdsFromRight, panelIdsFromBottom)
import Data.Maybe (mapMaybe)

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
        where header = BS.pack $ map fromIntegral [0, length idsToColors] 
              body :: BS.ByteString
              body = BS.concat $ map (\(pid, PanelUpdate r g b tt) -> encodeWord16 pid <> BS.pack (map fromIntegral [r, g, b, 0]) <> encodeWord16 tt) idsToColors
              encodeWord16 :: Int -> BS.ByteString
              encodeWord16 = BSL.toStrict . toLazyByteString . word16BE . fromIntegral 

--TODO: needs a way to stop as well
--TODO: check time since last message and make it wait until 100ms
doEffectsWhileMeasuring :: ControlStreamHandle -> MVar Int -> [PanelId] -> [Int -> Effect] -> [Effect] -> IO ()
doEffectsWhileMeasuring handle volMVar ids effectTriggers effects = do
    vol <- takeMVar volMVar
    --BUG: with current logic, priorities will be off from next cycle
    let withNewEffects = effects ++ map (\f -> f vol) effectTriggers
    let emptyEffectUpdate = replicate 15 (0, PanelUpdate 0 0 0 0)
    let layeredEffect = layerEffectUpdates $ map (headDef emptyEffectUpdate) withNewEffects
    sendByteString handle (createStreamingMsgFromMap layeredEffect)
    threadDelay (updateRateMs * 1000)
    let filteredEffects = filter (not . null) $ map (drop 1) withNewEffects
    doEffectsWhileMeasuring handle volMVar ids effectTriggers filteredEffects

continuousAllEffects :: NanoLeaf -> PanelLayout -> [String] -> IO () 
continuousAllEffects nl layout effectNames = do
    handle <- createSocket nl
    volMVar <- newEmptyMVar
    threadId <- forkIO (ALSA.volumeMeter volMVar)
    let triggers = effectNamesToTriggers layout effectNames
    doEffectsWhileMeasuring handle volMVar ids triggers []
    closeSocket handle
        where ids = panelIdsFromLeft layout

--TODO: Should only parse to Effect Data type (which should be added)
effectNamesToTriggers :: PanelLayout -> [String] -> [Int -> Effect]
effectNamesToTriggers pl = mapMaybe parseName 
    where ids = panelIdsFromLeft pl
          parseName x = 
            case x of "wave"  -> Just $ \v -> if v > 1000 then waveEffect R pl else []
                      "meter" -> Just $ volumeMeterEffect pl 
                      "flash" -> Just $ lightAllEffect pl
                      _       -> Nothing

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

