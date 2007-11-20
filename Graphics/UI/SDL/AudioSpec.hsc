#include "SDL.h"
#ifdef main
#undef main
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.SDL.AudioSpec
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Graphics.UI.SDL.AudioSpec where

{-
typedef struct{
  int freq;
  Uint16 format;
  Uint8 channels;
  Uint8 silence;
  Uint16 samples;
  Uint32 size;
  void (*callback)(void *userdata, Uint8 *stream, int len);
  void *userdata;
} SDL_AudioSpec;
-}

import Foreign

type RawCallback = Ptr () -> Ptr Word8 -> Int -> IO ()

data AudioSpec
    = AudioSpec
    { audioFreq :: Int
    , audioFormat :: Word16
    , audioChannels :: Word8
    , audioSilence :: Word8
    , audioSamples :: Word16
    , audioSize :: Word32
    , audioCallback :: (Ptr Word8 -> Int -> IO ())
    }

--foreign import ccall unsafe "wrapper" mkCallback :: RawCallback -> IO (FunPtr RawCallback)
{-
instance Storable AudioSpec where
    sizeOf _ = #{size SDL_AudioSpec}
    alignment _ = 4
    poke ptr (AudioSpec freq format channels silence samples size callback
-}
