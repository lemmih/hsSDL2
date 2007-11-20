#include "SDL.h"
#ifdef main
#undef main
#endif
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
    ) where

import Data.Word (Word16)

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


