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

bufSize :: Int
bufSize = 1000

microphoneDevice :: String
microphoneDevice = "plughw:0,0"

pulseAudioDevice :: String
pulseAudioDevice = "default"

inputFormat :: SoundFmt Int16
inputFormat = SoundFmt { sampleFreq = 8000 }

--TODO: Find out how often it actually measures(just take current time), and find way to limit to make it a parameter

--Takes a callback that may use the measured volume to affect nanoleafs
volumeMeter :: MVar Int -> IO ()
volumeMeter mVar = let source = alsaSoundSource microphoneDevice inputFormat
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

