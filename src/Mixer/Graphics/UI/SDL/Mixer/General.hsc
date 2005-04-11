#include <SDL_mixer.h>
-----------------------------------------------------------------------------
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

import Graphics.UI.SDL.General(unwrapMaybe, unwrapBool)
import Graphics.UI.SDL.Audio(AudioFormat(..), fromAudioFormat, toAudioFormat)

import Data.Word(Word16)
import Foreign(Ptr, Storable(peek), alloca)

defaultFrequency :: Int
defaultFrequency = 22050

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
