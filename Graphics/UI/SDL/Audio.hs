{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PackageImports #-}
module Graphics.UI.SDL.Audio
  ( -- * 'AudioFormat'
    AudioFormat

    -- * 'AudioSpec'
  , AudioSpec
  , audioSpecFreq
  , audioSpecFormat
  , audioSpecChannels
  , audioSpecSilence
  , audioSpecSize
  , audioSpecCallback

    -- * 'Channels'
  , Channels(..)

    -- * 'AudioDevice'
  , AudioDevice
  , getAudioDeviceNames
  , openAudioDevice
  , closeAudioDevice
  , LockState(..)
  , setAudioDeviceLocked
  , PlaybackState(..)
  , setAudioDevicePlaybackState
  -- , clearQueuedAudio
  , AudioDeviceStatus(..)
  , audioDeviceStatus
  , OpenDeviceSpec(..)
  , Changeable(..)
  , AudioDeviceUsage(..)

    -- * Audio Drivers
  , AudioDriver
  , audioDriverName
  , getAudioDrivers
  , currentAudioDriver

    -- * Explicit Initialization
  , audioInit
  , RawSDL.audioQuit
  ) where

import Control.Applicative
import Data.Traversable (for)
import Foreign
import Foreign.C
import Data.Text (Text)
import Data.Vector.Storable (Vector)

import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV

import qualified "sdl2" Graphics.UI.SDL.Audio as RawSDL
import qualified "sdl2" Graphics.UI.SDL.Enum as RawSDL
import qualified "sdl2" Graphics.UI.SDL.Types as RawSDL

newtype AudioFormat = AudioFormat { unAudioFormat :: Word16 }
  deriving (Eq, Ord, Show)

{-

audioFormatBitSize :: Lens' AudioFormat Word8
audioFormatFloat :: Lens' AudioFormat Bool
audioFormatBigEndian :: Lens' AudioFormat Bool
audioFormatSigned :: Lens' AudioFormat Bool

audioFormatU8 = AudioFormat 0 & audioFormatBitSize .~ 8
audioFormatS8 = audioFormatU8 & audioFormatSigned .~ True

audioFormatS16LSB = audioFormatS8 & audioFormatBitSize .~ 16
audioFormatS16MSB = audioFormatS16LSB & audioFormatBigEndian .~ True
audioFormatS16Sys = _
audioFormatS16 = audioFormatS16LSB
audioFormatU16LSB = audioFormatS16LSB & audioFormatSigned .~ False
audioFormatU16MSB = audioFormatS16MSB & audioFormatSigned .~ False
audioFormatU16Sys = _
audioFormatU16 = audioFormatU16LSB

audioFormatS32LSB = audioFormatS16LSB & audioFormatBitSize .~ 32
audioFormatS32MSB = audioFormatS16MSB & audioFormatBitSize .~ 32
audioFormatS32Sys = _
audioFormatS32 = audioFormatS32LSB

audioFormatF32LSB = audioFormatS32LSB & audioFormatFloat .~ True
audioFormatF32MSB = audioFormatS32MSB & audioFormatFloat .~ True
audioFormatF32Sys = _
audioFormatF32 = audioFormatF32LSB

-}

data Channels = Mono | Stereo | Quad | FivePointOne

data AudioSpec = AudioSpec
  { audioSpecFreq :: !CInt
  , audioSpecFormat :: !AudioFormat
  , audioSpecChannels :: !Channels
  , _audioSpecSilence :: !Word8
  , audioSpecSamples :: !Word16
  , _audioSpecSize :: !Word32
  , audioSpecCallback :: !(CInt -> IO (Vector Word8))
  }

audioSpecSilence :: AudioSpec -> Word8
audioSpecSilence = _audioSpecSilence

audioSpecSize :: AudioSpec -> Word32
audioSpecSize = _audioSpecSize

newtype AudioDevice = AudioDevice (RawSDL.AudioDeviceID)

getAudioDeviceNames :: AudioDeviceUsage -> IO (Maybe (V.Vector Text))
getAudioDeviceNames usage = do
  n <- RawSDL.getNumAudioDevices usage'
  if n == -1
    then return Nothing
    else fmap (Just . V.fromList) $
         for [0 .. (n - 1)] $ \i -> do
           cstr <- RawSDL.getAudioDeviceName i usage'
           Text.decodeUtf8 <$> BS.packCString cstr

  where usage' = encodeUsage usage

data AudioDeviceUsage = ForPlayback | ForCapture

encodeUsage :: Num a => AudioDeviceUsage -> a
encodeUsage usage =
  case usage of
    ForPlayback -> 0
    ForCapture -> 1

data OpenDeviceSpec = OpenDeviceSpec
  { openDeviceFreq :: !(Changeable CInt)
  , openDeviceFormat :: !(Changeable AudioFormat)
  , openDeviceChannels :: !(Changeable Channels)
  , openDeviceSamples :: !Word16
  , openDeviceCallback :: !(CInt -> IO (Vector Word8))
  , openDeviceUsage :: !AudioDeviceUsage
  , openDeviceName :: !(Maybe Text)
  }

data Changeable a
  = Mandate !a
  | Desire !a

foldChangeable :: (a -> b) -> (a -> b) -> Changeable a -> b
foldChangeable f _ (Mandate a) = f a
foldChangeable _ g (Desire a) = g a

unpackChangeable :: Changeable a -> a
unpackChangeable = foldChangeable id id

foreign import ccall "wrapper"
  mkAudioCallback :: (Ptr () -> Ptr Word8 -> CInt -> IO ())
     -> IO RawSDL.AudioCallback

openAudioDevice :: OpenDeviceSpec -> IO (AudioDevice, AudioSpec)
openAudioDevice OpenDeviceSpec{..} =
  maybeWith (BS.useAsCString . Text.encodeUtf8) openDeviceName $ \cDevName -> do
    cb <- mkAudioCallback $ \_ buffer len -> do
      v <- openDeviceCallback len
      let (vForeignPtr, len') = SV.unsafeToForeignPtr0 v
      withForeignPtr vForeignPtr $ \vPtr ->
        copyBytes buffer vPtr (min (fromIntegral len) (fromIntegral len'))
    with (desiredSpec cb) $ \desiredSpecPtr ->
      alloca $ \actualSpecPtr -> do
        devId <- RawSDL.openAudioDevice cDevName (encodeUsage openDeviceUsage) desiredSpecPtr actualSpecPtr changes
        actual <- peek actualSpecPtr
        let audioDevice = AudioDevice devId
            spec = AudioSpec { audioSpecFreq = RawSDL.audioSpecFreq actual
                             , audioSpecFormat = AudioFormat (RawSDL.audioSpecFormat actual)
                             , audioSpecChannels = unsafeReadChannels (RawSDL.audioSpecChannels actual)
                             , _audioSpecSilence = RawSDL.audioSpecSilence actual
                             , _audioSpecSize = RawSDL.audioSpecSize actual
                             , audioSpecSamples = RawSDL.audioSpecSamples actual
                             , audioSpecCallback = openDeviceCallback
                             }
        return (audioDevice, spec)

  where
  changes = foldl (.|.) 0 [ foldChangeable (const audioAllowFrequencyChange) (const 0) openDeviceFreq
                          , foldChangeable (const audioAllowFormatChange) (const 0) openDeviceFormat
                          , foldChangeable (const audioAllowChannelsChange) (const 0) openDeviceChannels
                          ]

  -- XXX Move to RawSDL
  audioAllowFrequencyChange = 0
  audioAllowFormatChange = 0
  audioAllowChannelsChange = 0

  channelsToWord8 Mono = 1
  channelsToWord8 Stereo = 2
  channelsToWord8 Quad = 4
  channelsToWord8 FivePointOne = 6

  unsafeReadChannels 1 = Mono
  unsafeReadChannels 2 = Stereo
  unsafeReadChannels 4 = Quad
  unsafeReadChannels 6 = FivePointOne
  unsafeReadChannels _ = error "openAudioDevice.unsafeReadChannels: Unexpected argument"

  desiredSpec cb = RawSDL.AudioSpec
    { RawSDL.audioSpecFreq = unpackChangeable openDeviceFreq
    , RawSDL.audioSpecFormat = unAudioFormat (unpackChangeable openDeviceFormat)
    , RawSDL.audioSpecChannels = channelsToWord8 (unpackChangeable openDeviceChannels)
    , RawSDL.audioSpecSilence = 0
    , RawSDL.audioSpecSize = 0
    , RawSDL.audioSpecSamples = openDeviceSamples
    , RawSDL.audioSpecCallback = cb
    , RawSDL.audioSpecUserdata = nullPtr
    }

closeAudioDevice :: AudioDevice -> IO ()
closeAudioDevice (AudioDevice d) = RawSDL.closeAudioDevice d

data LockState = Locked | Unlocked

setAudioDeviceLocked :: AudioDevice -> LockState -> IO ()
setAudioDeviceLocked (AudioDevice d) Locked = RawSDL.lockAudioDevice d
setAudioDeviceLocked (AudioDevice d) Unlocked = RawSDL.unlockAudioDevice d

data PlaybackState = Pause | Play

setAudioDevicePlaybackState :: AudioDevice -> PlaybackState -> IO ()
setAudioDevicePlaybackState (AudioDevice d) Pause = RawSDL.pauseAudioDevice d 1
setAudioDevicePlaybackState (AudioDevice d) Play = RawSDL.pauseAudioDevice d 0

data AudioDeviceStatus = Playing | Paused | Stopped

audioDeviceStatus :: AudioDevice -> IO AudioDeviceStatus
audioDeviceStatus (AudioDevice d) = unsafeReadStatus <$> RawSDL.getAudioDeviceStatus d
  where
  unsafeReadStatus n
    | n == RawSDL.audioStatusPlaying = Playing
    | n == RawSDL.audioStatusStopped = Stopped
    | n == RawSDL.audioStatusPaused = Paused
    | otherwise = error "audioDeviceStatus: Unknown argument"

-- clearQueuedAudio :: AudioDevice -> IO ()
-- clearQueuedAudio (AudioDevice d) = RawSDL.clearQueuedAudio d

newtype AudioDriver = AudioDriver Text

audioDriverName :: AudioDriver -> Text
audioDriverName (AudioDriver t) = t

getAudioDrivers :: IO (V.Vector AudioDriver)
getAudioDrivers = do
  n <- RawSDL.getNumAudioDrivers
  fmap V.fromList $
    for [0 .. (n - 1)] $ \i -> do
      cstr <- RawSDL.getAudioDriver i
      AudioDriver . Text.decodeUtf8 <$> BS.packCString cstr

audioInit :: AudioDriver -> IO CInt
audioInit (AudioDriver n) = BS.useAsCString (Text.encodeUtf8 n) RawSDL.audioInit

currentAudioDriver :: IO (Maybe Text)
currentAudioDriver =
  maybePeek (fmap Text.decodeUtf8 . BS.packCString) =<< RawSDL.getCurrentAudioDriver

{-

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


--------------------------------------------------------------------------------
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

-}
