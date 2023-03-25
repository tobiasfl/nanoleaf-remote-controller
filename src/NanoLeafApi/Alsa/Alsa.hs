module NanoLeafApi.Alsa.Alsa
    (volumeMeter
    ) where

import Sound.ALSA.PCM
         (SoundFmt(SoundFmt), sampleFreq, soundSourceRead,
          SoundSource, alsaSoundSource, withSoundSource)

import Foreign
         (allocaArray, peekArray,
          Storable, Ptr, castPtr, )
import Data.Int (Int16, )
import Control.Concurrent.MVar (MVar, tryPutMVar)
import Data.Time.Clock (getCurrentTime, diffUTCTime, UTCTime)

bufSize :: Int
bufSize = 1000

microphoneDevice :: String
microphoneDevice = "plughw:0,0"

pulseAudioDevice :: String
pulseAudioDevice = "default"

inputFormat :: SoundFmt Int16
inputFormat = SoundFmt { sampleFreq = 16000 } --example was 8000


--Takes a callback that may use the measured volume to affect nanoleafs
volumeMeter :: MVar Int -> IO ()
volumeMeter mVar = let source = alsaSoundSource pulseAudioDevice inputFormat
                in allocaArray     bufSize $ \buf  ->
                   withSoundSource source  $ \handle -> 
                       loop source handle bufSize buf mVar

-- | assumes that the file contains numbers in the host's byte order
loop :: SoundSource h Int16 -> h Int16 -> Int -> Ptr Int16 -> MVar Int -> IO ()
loop src handle n buf mVar =
    do n' <- soundSourceRead src handle (castPtr buf) n--Read current 'value' of sound source?
       avg <- avgBuf buf n' --n' is just how many bytes were read into the buffer I think
       putStrLn $ "Avg volume" ++ show avg
       putStrLn (replicate (avg `div` 20) '*')
       tryPutMVar mVar avg
       loop src handle n buf mVar

avgBuf :: (Storable a, Integral a) => Ptr a -> Int -> IO Int
avgBuf buf n = do xs <- peekArray n buf--peekArray converts to a Haskell list
                  let xs' = map (fromIntegral . abs) xs :: [Int]
                  return $ sum xs' `div` n

