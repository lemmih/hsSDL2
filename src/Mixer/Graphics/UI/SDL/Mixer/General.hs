{-# OPTIONS_GHC -optc-D_REENTRANT #-}
{-# INCLUDE <SDL_mixer.h> #-}
{-# LINE 1 "General.hsc" #-}
-----------------------------------------------------------------------------
{-# LINE 2 "General.hsc" #-}
-- |
-- Module      :  Graphics.UI.SDL.Mixer.General
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Graphics.UI.SDL.Mixer.General
    ( AudioFormat (..)
    , tryOpenAudio
    , openAudio
    , closeAudio
    , tryQuerySpec
    , querySpec
    , defaultFrequency
    ) where

import Foreign
import Data.Word

import Graphics.UI.SDL.General


{-# LINE 28 "General.hsc" #-}

defaultFrequency :: Int
defaultFrequency = 22050

data AudioFormat
    = AudioU8
    | AudioS8
    | AudioU16LBS
    | AudioS16LBS
    | AudioU16MBS
    | AudioS16MBS
    | AudioU16Sys
    | AudioS16Sys
      deriving (Show,Eq,Ord,Enum)

fromAudioFormat :: AudioFormat -> Word16
fromAudioFormat AudioU8 = 0x0008
fromAudioFormat AudioS8 = 0x8008
fromAudioFormat AudioU16LBS = 0x0010
fromAudioFormat AudioS16LBS = 0x8010
fromAudioFormat AudioU16MBS = 0x1010
fromAudioFormat AudioS16MBS = 0x9010

{-# LINE 51 "General.hsc" #-}
fromAudioFormat AudioU16Sys = 0x0010
fromAudioFormat AudioS16Sys = 0x8010

{-# LINE 57 "General.hsc" #-}

toAudioFormat :: Word16 -> AudioFormat
toAudioFormat 0x0008 = AudioU8
toAudioFormat 0x8008 = AudioS8
toAudioFormat 0x0010 = AudioU16LBS
toAudioFormat 0x8010 = AudioS16LBS
toAudioFormat 0x1010 = AudioU16MBS
toAudioFormat 0x9010 = AudioS16MBS

-- int Mix_OpenAudio(int frequency, Uint16 format, int channels, int chunksize)
foreign import ccall unsafe "Mix_OpenAudio" mixOpenAudio :: Int -> Word16 -> Int -> Int -> IO Int
tryOpenAudio :: Int -> AudioFormat -> Int -> Int -> IO Bool
tryOpenAudio frequency format channels chunksize
    = fmap (0==) (mixOpenAudio frequency (fromAudioFormat format) channels chunksize)

openAudio :: Int -> AudioFormat -> Int -> Int -> IO ()
openAudio frequency format channels chunksize
    = unwrapBool "Mix_OpenAudio" (tryOpenAudio frequency format channels chunksize)

-- void Mix_CloseAudio()
foreign import ccall unsafe "Mix_CloseAudio" closeAudio :: IO ()

-- int Mix_QuerySpec(int *frequency, Uint16 *format, int *channels)
foreign import ccall unsafe "Mix_QuerySpec" mixQuerySpec :: Ptr Int -> Ptr Word16 -> Ptr Int -> IO Int
tryQuerySpec :: IO (Maybe (Int,AudioFormat,Int))
tryQuerySpec
    = alloca $ \freq ->
      alloca $ \format ->
      alloca $ \channels ->
      do ret <- mixQuerySpec freq format channels
         case ret of
           0 -> return Nothing
           _ -> do [f,c] <- mapM peek [freq,channels]
                   fm <- peek format
                   return (Just (f,toAudioFormat fm,c))

querySpec :: IO (Int,AudioFormat,Int)
querySpec = unwrapMaybe "Mix_QuerySpec" tryQuerySpec
