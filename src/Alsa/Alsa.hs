module Alsa.Alsa
    (volumeMeter
    ) where

import Sound.ALSA.PCM
         (SoundFmt(SoundFmt), sampleFreq, soundSourceRead,
          SoundSource, alsaSoundSource, withSoundSource, )

import Foreign
         (allocaArray, peekArray,
          Storable, Ptr, castPtr, )
import Data.Int (Int16, )

bufSize :: Int
bufSize = 1000

microphoneDevice :: String
microphoneDevice = "plughw:0,0"--TODO: BUG, this captures microphone as well!(Might actually be ONLY microphone at this point)

pulseAudioDevice :: String
pulseAudioDevice = "default"

inputFormat :: SoundFmt Int16
inputFormat = SoundFmt { sampleFreq = 10000 }

volumeMeter :: IO ()
volumeMeter = let source = alsaSoundSource microphoneDevice inputFormat
                in allocaArray     bufSize $ \buf  ->
                   withSoundSource source  $ \from ->
                       loop source from bufSize buf

-- | assumes that the file contains numbers in the host's byte order
loop :: SoundSource h Int16 -> h Int16 -> Int -> Ptr Int16 -> IO ()
loop src h n buf =
    do n' <- soundSourceRead src h (castPtr buf) n
       avg <- avgBuf buf n'
       putStrLn (replicate (avg `div` 20) '*')
       loop src h n buf

avgBuf :: (Storable a, Integral a) => Ptr a -> Int -> IO Int
avgBuf buf n = do xs <- peekArray n buf
                  let xs' = map (fromIntegral . abs) xs :: [Int]
                  return $ sum xs' `div` n
