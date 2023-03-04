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

bufSize :: Int
bufSize = 1000

microphoneDevice :: String
microphoneDevice = "plughw:0,0"

pulseAudioDevice :: String
pulseAudioDevice = "default"

inputFormat :: SoundFmt Int16
inputFormat = SoundFmt { sampleFreq = 8000 }

--TODO: somehow connect so that volume level triggers a streaming message to NanoLeafs
--TODO: Find out how often it actually measures, and find way to limit to make it a parameter

volumeMeter :: (Int -> IO ()) -> IO ()
volumeMeter cb = let source = alsaSoundSource pulseAudioDevice inputFormat
                in allocaArray     bufSize $ \buf  ->
                   withSoundSource source  $ \handle -> 
                       loop source handle bufSize buf cb

-- | assumes that the file contains numbers in the host's byte order
loop :: SoundSource h Int16 -> h Int16 -> Int -> Ptr Int16 -> (Int -> IO ()) -> IO ()
loop src handle n buf cb =
    do n' <- soundSourceRead src handle (castPtr buf) n--Read current 'value' of sound source?
       avg <- avgBuf buf n' --n' is just how many bytes were read into the buffer I think
       putStrLn $ "Avg volume" ++ show avg
       putStrLn (replicate (avg `div` 20) '*')
       cb avg
       loop src handle n buf cb

avgBuf :: (Storable a, Integral a) => Ptr a -> Int -> IO Int
avgBuf buf n = do xs <- peekArray n buf--peekArray converts to a Haskell list
                  let xs' = map (fromIntegral . abs) xs :: [Int]
                  return $ sum xs' `div` n

