{-# LANGUAGE RecordWildCards #-}
#include "SDL.h"
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.SDL.Audio
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Graphics.UI.SDL.Audio
    ( AudioFormat (..)
    , fromAudioFormat
    , toAudioFormat
    
      -- * AudioSpec
    , AudioSpec (..)

      -- * WAV files
    , loadWAV

      -- * AudioCVT
    , AudioCVT(..)
    , AudioFilter
    , buildAudioCVT
    , convertAudio
    
      -- * Audio Devices
    , AudioDevice
    , AudioStatus(..)
    , AudioDeviceUsage (..)
    , getAudioDeviceName
    , getAudioDeviceStatus
    , getAudioDriver
    , getAudioStatus
    , getCurrentAudioDriver
    , getNumAudioDevices
    , getNumAudioDrivers
    , lockAudio
    , lockAudioDevice
    , openAudioDevice 
    , pauseAudioDevice
    , unlockAudio
    , unlockAudioDevice

    , mixAudio
    , mixAudioFormat
    , audioInit
    , audioQuit
    , closeAudio
    , closeAudioDevice
    , openAudio
    , pauseAudio
    ) where

import Control.Applicative
import Control.Monad ((>=>))
import Foreign
import Foreign.C
import Data.ByteString
import Data.Maybe (fromMaybe)
import Data.Vector.Storable (Vector)
import Graphics.UI.SDL.Types
import Graphics.UI.SDL.General
import Graphics.UI.SDL.Utilities (fatalSDLNull, fatalSDLBool)

import qualified Data.ByteString.Internal as BI
import qualified Data.Vector.Storable as V
import qualified Graphics.UI.SDL.RWOps as RWOps

data AudioFormat
    = AudioU8
    | AudioS8
    | AudioU16LSB
    | AudioS16LSB
    | AudioU16MSB
    | AudioS16MSB
    | AudioU16Sys
    | AudioS16Sys
      deriving (Show,Eq,Ord,Enum)

fromAudioFormat :: AudioFormat -> Word16
fromAudioFormat AudioU8 = #{const AUDIO_U8}
fromAudioFormat AudioS8 = #{const AUDIO_S8}
fromAudioFormat AudioU16LSB = #{const AUDIO_U16LSB}
fromAudioFormat AudioS16LSB = #{const AUDIO_S16LSB}
fromAudioFormat AudioU16MSB = #{const AUDIO_U16MSB}
fromAudioFormat AudioS16MSB = #{const AUDIO_S16MSB}
fromAudioFormat AudioU16Sys = #{const AUDIO_U16SYS}
fromAudioFormat AudioS16Sys = #{const AUDIO_S16SYS}

toAudioFormat :: Word16 -> AudioFormat
toAudioFormat #{const AUDIO_U8} = AudioU8
toAudioFormat #{const AUDIO_S8} = AudioS8
toAudioFormat #{const AUDIO_U16LSB} = AudioU16LSB
toAudioFormat #{const AUDIO_S16LSB} = AudioS16LSB
toAudioFormat #{const AUDIO_U16MSB} = AudioU16MSB
toAudioFormat #{const AUDIO_S16MSB} = AudioS16MSB
toAudioFormat _ = error "Graphics.UI.SDL.Audio.toAudioFormat: bad argument"

data AudioSpec = AudioSpec { audioSpecFreq :: #{type int}
                           , audioSpecFormat :: AudioFormat
                           , audioSpecChannels :: #{type Uint8}
                           , audioSpecSilence :: #{type Uint8}
                           , audioSpecSamples :: #{type Uint16}
                           , audioSpecSize :: #{type Uint32}
                           , audioSpecCallback :: Maybe (#{type int} -> IO (Vector #{type Uint8}))
                           }

instance Show AudioSpec where
  show AudioSpec {..} = unlines [ show audioSpecFreq, show audioSpecFormat, show audioSpecChannels, show audioSpecSilence, show audioSpecSamples, show audioSpecSize ]

foreign import ccall "wrapper"
  mkAudioCallback :: (Ptr () -> Ptr (#{type Uint8}) -> #{type int} -> IO ())
     -> IO (FunPtr (Ptr () -> Ptr (#{type Uint8}) -> #{type int} -> IO ()))

instance Storable AudioSpec where
  sizeOf = const #{size SDL_AudioSpec}
  
  alignment = const 4
  
  poke ptr AudioSpec{..} = do
    #{poke SDL_AudioSpec, freq} ptr audioSpecFreq
    #{poke SDL_AudioSpec, format} ptr (fromAudioFormat audioSpecFormat)
    #{poke SDL_AudioSpec, channels} ptr audioSpecChannels
    #{poke SDL_AudioSpec, silence} ptr audioSpecSilence
    #{poke SDL_AudioSpec, samples} ptr audioSpecSamples
    #{poke SDL_AudioSpec, size} ptr audioSpecSize
    
    cb <- mkAudioCallback $ \_ buffer len -> do
      v <- fromMaybe (return . flip V.replicate 0 . fromIntegral) audioSpecCallback $ len
      let (vForeignPtr, len') = V.unsafeToForeignPtr0 v
      withForeignPtr vForeignPtr $ \vPtr ->
        copyBytes buffer vPtr (min (fromIntegral len) (fromIntegral len'))

    #{poke SDL_AudioSpec, callback} ptr cb

  peek ptr = AudioSpec
    <$> #{peek SDL_AudioSpec, freq} ptr
    <*> (toAudioFormat <$> #{peek SDL_AudioSpec, format} ptr)
    <*> #{peek SDL_AudioSpec, channels} ptr
    <*> #{peek SDL_AudioSpec, silence} ptr
    <*> #{peek SDL_AudioSpec, samples} ptr
    <*> #{peek SDL_AudioSpec, size} ptr
    <*> pure Nothing

type AudioFilter
   = FunPtr (Ptr AudioCVT -> #{type SDL_AudioFormat} -> IO ())

data AudioCVT
   = AudioCVT { audioCVTNeeded :: Bool
              , audioCVTSrcFormat :: AudioFormat
              , audioCVTDstFormat :: AudioFormat
              , audioCVTRateIncr :: #{type double}
              , audioCVTBuf :: Ptr #{type Uint8}
              , audioCVTLen :: #{type int}
              , audioCVTLenCVT :: #{type int}
              , audioCVTLenMult :: #{type int}
              , audioCVTLenRatio :: #{type double}
              , audioCVTFilters :: Ptr AudioFilter
              , audioCVTFilterIndex :: #{type int}
              }
  deriving (Eq, Show)

instance Storable AudioCVT where
  sizeOf    = const #{size SDL_AudioCVT}
  alignment = const 4
  poke ptr AudioCVT{..} = do
    #{poke SDL_AudioCVT, needed} ptr ((fromBool :: Bool -> #{type int}) audioCVTNeeded)
    #{poke SDL_AudioCVT, src_format} ptr (fromAudioFormat audioCVTSrcFormat)
    #{poke SDL_AudioCVT, dst_format} ptr (fromAudioFormat audioCVTDstFormat)
    #{poke SDL_AudioCVT, rate_incr} ptr audioCVTRateIncr
    #{poke SDL_AudioCVT, buf} ptr audioCVTBuf
    #{poke SDL_AudioCVT, len} ptr audioCVTLen
    #{poke SDL_AudioCVT, len_cvt} ptr audioCVTLenCVT
    #{poke SDL_AudioCVT, len_mult} ptr audioCVTLenMult
    #{poke SDL_AudioCVT, len_ratio} ptr audioCVTLenRatio
    #{poke SDL_AudioCVT, filters} ptr audioCVTFilters
    #{poke SDL_AudioCVT, filter_index} ptr audioCVTFilterIndex
  peek ptr = AudioCVT
    <$> ((toBool :: #{type int} -> Bool) <$> #{peek SDL_AudioCVT, needed} ptr)
    <*> (toAudioFormat <$> #{peek SDL_AudioCVT, src_format} ptr)
    <*> (toAudioFormat <$> #{peek SDL_AudioCVT, dst_format} ptr)
    <*> #{peek SDL_AudioCVT, rate_incr} ptr
    <*> #{peek SDL_AudioCVT, buf} ptr -- TODO convert to ByteString
    <*> #{peek SDL_AudioCVT, len} ptr
    <*> #{peek SDL_AudioCVT, len_cvt} ptr
    <*> #{peek SDL_AudioCVT, len_mult} ptr
    <*> #{peek SDL_AudioCVT, len_ratio} ptr
    <*> #{peek SDL_AudioCVT, filters} ptr
    <*> #{peek SDL_AudioCVT, filter_index} ptr

foreign import ccall unsafe "SDL_BuildAudioCVT"
  sdlBuildAudioCVT :: Ptr AudioCVT -> #{type SDL_AudioFormat} -> #{type Uint8}
                      -> #{type int} -> #{type SDL_AudioFormat} -> #{type Uint8}
                      -> #{type int} -> IO #{type int}

buildAudioCVT :: AudioFormat -> Int -> Int -> AudioFormat -> Int -> Int -> IO (Maybe AudioCVT)
buildAudioCVT src_fmt src_channels src_rate dst_fmt dst_channels dst_rate =
  alloca $ \cvt' -> do
    let src_fmt'      = fromAudioFormat src_fmt
        src_channels' = fromIntegral src_channels
        src_rate'     = fromIntegral src_rate
        dst_fmt'      = fromAudioFormat dst_fmt
        dst_channels' = fromIntegral dst_channels
        dst_rate'     = fromIntegral dst_rate
    ret <- sdlBuildAudioCVT cvt' src_fmt' src_channels' src_rate' dst_fmt' dst_channels' dst_rate'
    if ret == -1
      then return Nothing
      else (peek cvt') >>= (return . Just)

foreign import ccall unsafe "SDL_ConvertAudio"
  sdlConvertAudio :: Ptr AudioCVT -> IO #{type int}

convertAudio :: AudioCVT -> IO ()
convertAudio cvt =
  with cvt $ \cvt' -> do
    ret <- sdlConvertAudio cvt'
    handleErrorI "sdlConvertAudio" ret (const $ return ())

foreign import ccall unsafe "&SDL_FreeWAV"
  sdlFreeWAV_finalizer :: FunPtr (Ptr (#{type Uint8}) -> IO ())

foreign import ccall unsafe "SDL_LoadWAV_RW"
  sdlLoadWAV :: Ptr RWopsStruct -> #{type int} -> Ptr AudioSpec -> Ptr (Ptr #{type Uint8}) -> Ptr (#{type Uint32}) -> IO (Ptr AudioSpec)

loadWAV :: FilePath -> AudioSpec -> IO (Vector Word8, AudioSpec)
loadWAV filePath desiredSpec =
  RWOps.withFile filePath "r" $ \rwops ->
  withForeignPtr rwops $ \cRwops ->
  with desiredSpec $ \desiredSpecPtr ->
  alloca $ \outputBufferSizePtr -> do
    outputBufferPtr <- malloc
    actualSpecBuffer <-  throwIfNull "loadWAV: failed to load WAV" $
      sdlLoadWAV cRwops 0 desiredSpecPtr outputBufferPtr outputBufferSizePtr

    peek actualSpecBuffer >>= \actualSpec -> do
      outputBuffer <- peek outputBufferPtr
      foreignAudioBuffer <- newForeignPtr sdlFreeWAV_finalizer outputBuffer
      outputBufferV <- V.unsafeFromForeignPtr0 foreignAudioBuffer . fromIntegral <$> peek outputBufferSizePtr
      return (outputBufferV, actualSpec)

foreign import ccall unsafe "SDL_OpenAudioDevice"
  sdlOpenAudioDevice :: CString -> #{type int} -> Ptr AudioSpec -> Ptr AudioSpec -> #{type int}
                     -> IO (#{type SDL_AudioDeviceID})

data AudioDeviceUsage = ForPlayback | ForCapture

newtype AudioDevice = AudioDevice (#{type SDL_AudioDeviceID})

openAudioDevice :: Maybe String -> AudioDeviceUsage -> AudioSpec -> [a] -> IO (AudioDevice, AudioSpec)
openAudioDevice deviceName usage desiredSpec _ =
  maybeWith withCString deviceName $ \cDevName ->
  with desiredSpec $ \desiredSpecPtr ->
  alloca $ \actualSpecPtr -> do
    devId <- sdlOpenAudioDevice cDevName (encodeUsage usage) desiredSpecPtr actualSpecPtr
               (#{const SDL_AUDIO_ALLOW_ANY_CHANGE})
    actualSpec <- peek actualSpecPtr
    return (AudioDevice devId, actualSpec)

foreign import ccall unsafe "SDL_OpenAudio"
  sdlOpenAudio :: Ptr AudioSpec -> Ptr AudioSpec -> IO #{type int}

openAudio :: AudioSpec -> IO (Maybe AudioSpec)
openAudio desiredSpec =
   with desiredSpec $ \desiredSpecPtr ->
   alloca $ \obtainedSpec -> do
     fatalSDLBool "SDL_OpenAudio" (sdlOpenAudio desiredSpecPtr obtainedSpec)
     case obtainedSpec of
       nullPtr -> return Nothing
       _       -> peek obtainedSpec >>= return . Just

encodeUsage :: AudioDeviceUsage -> #{type int}
encodeUsage ForPlayback = 0
encodeUsage ForCapture = 1

foreign import ccall unsafe "SDL_PauseAudioDevice"
  sdlPauseAudioDevice :: #{type SDL_AudioDeviceID} -> #{type int} -> IO ()

pauseAudioDevice :: AudioDevice -> Bool -> IO ()
pauseAudioDevice (AudioDevice dId) paused =
  sdlPauseAudioDevice dId (if paused then 1 else 0)
 
--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_LockAudio"
  lockAudio :: IO ()
  
foreign import ccall unsafe "SDL_UnlockAudio"
  unlockAudio :: IO ()
  
--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_LockAudioDevice"
  sdlLockAudioDevice :: #{type SDL_AudioDeviceID} -> IO ()

lockAudioDevice :: AudioDevice -> IO ()
lockAudioDevice (AudioDevice dId) = sdlLockAudioDevice dId

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_UnlockAudioDevice"
  sdlUnlockAudioDevice :: #{type SDL_AudioDeviceID} -> IO ()

unlockAudioDevice :: AudioDevice -> IO ()
unlockAudioDevice (AudioDevice dId) = sdlUnlockAudioDevice dId

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_GetNumAudioDrivers"
  getNumAudioDrivers :: IO #{type int}

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_GetAudioDriver"
  sdlGetAudioDriver :: #{type int} -> IO CString

getAudioDriver :: #{type int} -> IO String
getAudioDriver = sdlGetAudioDriver >=> peekCString

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_GetCurrentAudioDriver"
  sdlGetCurrentAudioDriver :: IO CString

getCurrentAudioDriver :: IO (Maybe String)
getCurrentAudioDriver = sdlGetCurrentAudioDriver >>= maybePeek peekCString

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_GetNumAudioDevices"
  sdlGetNumAudioDevices :: #{type int} -> IO #{type int}

getNumAudioDevices :: AudioDeviceUsage -> IO #{type int}
getNumAudioDevices = sdlGetNumAudioDevices . encodeUsage

--------------------------------------------------------------------------------
data AudioStatus = AudioStopped | AudioPlaying | AudioPaused

decodeAudioStatus :: #{type SDL_AudioStatus} -> AudioStatus
decodeAudioStatus #{const SDL_AUDIO_STOPPED} = AudioStopped
decodeAudioStatus #{const SDL_AUDIO_PLAYING} = AudioPlaying
decodeAudioStatus #{const SDL_AUDIO_PAUSED} = AudioPaused
decodeAudioStatus i = error $ "Unexpected SDL_AudioStatus: " ++ show i

foreign import ccall unsafe "SDL_GetAudioStatus"
  sdlGetAudioStatus :: IO #{type SDL_AudioStatus}

getAudioStatus :: IO AudioStatus
getAudioStatus = decodeAudioStatus <$> sdlGetAudioStatus

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_GetAudioDeviceName"
  sdlGetAudioDeviceName :: #{type int} -> #{type int} -> IO CString

getAudioDeviceName :: AudioDeviceUsage -> #{type int} -> IO String
getAudioDeviceName usage index =
  fatalSDLNull "SDL_GetAudioDeviceName"
    (sdlGetAudioDeviceName (encodeUsage usage) index) >>= peekCString

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_GetAudioDeviceStatus"
  sdlGetAudioDeviceStatus :: #{type SDL_AudioDeviceID} -> IO #{type SDL_AudioStatus}

getAudioDeviceStatus :: AudioDevice -> IO AudioStatus
getAudioDeviceStatus (AudioDevice dId) =
  decodeAudioStatus <$> sdlGetAudioDeviceStatus dId

foreign import ccall unsafe "SDL_MixAudio"
  sdlMixAudio :: Ptr #{type Uint8} -> Ptr #{type Uint8} -> #{type Uint32} -> #{type int} -> IO ()

mixAudio :: ByteString -> ByteString -> Int -> IO ()
mixAudio xbs ybs volume =
  let (ybs', _, _)   = BI.toForeignPtr ybs
      (xbs', _, len) = BI.toForeignPtr xbs
  in withForeignPtr ybs' $ \ybs'' ->
     withForeignPtr xbs' $ \xbs'' ->
       let len' = fromIntegral $ len
           vol' = fromIntegral volume
       in sdlMixAudio xbs'' ybs'' len' vol'

foreign import ccall unsafe "SDL_MixAudioFormat"
  sdlMixAudioFormat :: Ptr #{type Uint8} -> Ptr #{type Uint8} -> #{type SDL_AudioFormat} -> #{type Uint32} -> #{type int} -> IO ()

mixAudioFormat :: ByteString -> ByteString -> AudioFormat -> Int -> IO ()
mixAudioFormat xbs ybs aufmt volume =
  let (ybs', _, _)   = BI.toForeignPtr ybs
      (xbs', _, len) = BI.toForeignPtr xbs
  in withForeignPtr ybs' $ \ybs'' ->
     withForeignPtr xbs' $ \xbs'' ->
       let len' = fromIntegral $ len
           vol' = fromIntegral volume
           fmt' = fromAudioFormat aufmt
       in sdlMixAudioFormat xbs'' ybs'' fmt' len' vol'

foreign import ccall unsafe "SDL_AudioInit"
  sdlAudioInit :: CString -> IO #{type int}

audioInit :: String -> IO ()
audioInit driver_name =
  withCString driver_name $ \cstr ->
    fatalSDLBool "SDL_AudioInit" (sdlAudioInit cstr)

foreign import ccall unsafe "SDL_AudioQuit"
  audioQuit :: IO ()

foreign import ccall unsafe "SDL_CloseAudio"
  closeAudio :: IO ()

foreign import ccall unsafe "SDL_CloseAudioDevice"
  sdlCloseAudioDevice :: #{type SDL_AudioDeviceID} -> IO ()

closeAudioDevice :: AudioDevice -> IO ()
closeAudioDevice (AudioDevice dev) = sdlCloseAudioDevice dev

foreign import ccall unsafe "SDL_PauseAudio"
  sdlPauseAudio :: #{type int} -> IO ()

-- | True to pause. False to unpause.
pauseAudio :: Bool -> IO ()
pauseAudio True  = sdlPauseAudio 1
pauseAudio False = sdlPauseAudio 0

