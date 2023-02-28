module NanoLeafApi.Alsa.Alsa
    (volumeMeter,
    volumeMeterTime
    ) where

import Sound.ALSA.PCM
         (SoundFmt(SoundFmt), sampleFreq, soundSourceRead,
          SoundSource, alsaSoundSource, withSoundSource, alsaSoundSourceTime,
          SoundBufferTime (SoundBufferTime, bufferTime, periodTime))

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

volumeMeter :: IO ()
volumeMeter = let source = alsaSoundSource microphoneDevice inputFormat
                in allocaArray     bufSize $ \buf  ->
                   withSoundSource source  $ \handle -> 
                       loop source handle bufSize buf (const (print "empty cb"))

--bufferTime and periodTime are likely in ms 
--periodTime specifies after which interval the thread gets wakened up when waiting 
--for sound buffer to become non-empty
--TODO: the params don't seem to correspond with what happens so I am missing something
volumeMeterTime :: (Int -> IO ()) -> IO ()
volumeMeterTime cb = let time = (SoundBufferTime {bufferTime = 100, periodTime = 100})
                         source = alsaSoundSourceTime microphoneDevice inputFormat time
                        in allocaArray     bufSize $ \buf  ->
                           withSoundSource source  $ \handle -> 
                               loop source handle bufSize buf cb

-- | assumes that the file contains numbers in the host's byte order
loop :: SoundSource h Int16 -> h Int16 -> Int -> Ptr Int16 -> (Int -> IO ()) -> IO ()
loop src handle n buf cb =
    do n' <- soundSourceRead src handle (castPtr buf) n--Read current 'value' of sound source?
       avg <- avgBuf buf n'
       putStrLn (replicate (avg `div` 20) '*')
       cb avg
       loop src handle n buf cb

avgBuf :: (Storable a, Integral a) => Ptr a -> Int -> IO Int
avgBuf buf n = do xs <- peekArray n buf--peekArray converts to a Haskell list
                  let xs' = map (fromIntegral . abs) xs :: [Int]
                  return $ sum xs' `div` n

--TODO: consider separating streaming part from rest of nanoleafapi and combining it with Alsa
