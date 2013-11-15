{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}
#include "SDL.h"
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.SDL.Keysym
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Graphics.UI.SDL.Keysym where

import Control.Applicative
import Data.Char (chr, ord)
import Foreign
import Foreign.Ptr
import Foreign.Storable

import Graphics.UI.SDL.Keycode
import Graphics.UI.SDL.Meta
import Graphics.UI.SDL.Enums
import Graphics.UI.SDL.Utilities

$(makeEnum "Modifier" modifierNames)
$(makeEnum "Scancode" scancodeNames)

instance Storable Scancode where
    sizeOf = const #{size SDL_Scancode}
    alignment = const 4
    poke ptr sc = poke (castPtr ptr) (fromEnum sc)
    peek ptr = toEnum <$> peek (castPtr ptr)

data Keysym = Keysym { keyScancode :: Scancode
                     , keyKeycode :: Keycode
                     , keyModifiers :: Word16
                     }
  deriving (Eq, Show)

instance Storable Keysym where
    sizeOf = const #{size SDL_Keysym}
    alignment = const 4
    poke ptr (Keysym scancode keycode mods) = do
        #{poke SDL_Keysym, sym} ptr scancode
        #{poke SDL_Keysym, mod} ptr mods
        #{poke SDL_Keysym, scancode} ptr scancode
    peek ptr = Keysym
        <$> (toEnum <$> #{peek SDL_Keysym, sym} ptr)
        <*> (#{peek SDL_Keysym, mod} ptr)
        <*> (peek =<< #{peek SDL_Keysym, scancode} ptr)
