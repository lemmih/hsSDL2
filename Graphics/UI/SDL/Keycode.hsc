{-# LANGUAGE TemplateHaskell #-}
#include "SDL.h"
module Graphics.UI.SDL.Keycode where

import Prelude hiding (Either(Left,Right))
import Foreign.Ptr
import Foreign.Storable

import Graphics.UI.SDL.Meta
import Graphics.UI.SDL.Enums

$(makeEnum "Keycode" keycodeNames)

instance Storable Keycode where
    sizeOf = const #{size SDL_Keycode}
    alignment = const 4
    poke ptr kc = poke (castPtr ptr) (fromEnum kc)
    peek ptr = toEnum `fmap` peek (castPtr ptr)
