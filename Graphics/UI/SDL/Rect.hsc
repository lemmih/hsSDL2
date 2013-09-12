#include "SDL.h"
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.SDL.Video
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Graphics.UI.SDL.Rect where

import Foreign (Storable(poke, sizeOf, alignment, peekByteOff, pokeByteOff,
                         peek))
import Foreign.C
import Data.Word (Word16)
import Data.Int (Int16)

data Rect
    = Rect
    { rectX, rectY :: Int,  -- Actually Int16
      rectW, rectH :: Int } -- Actually Word16
    deriving (Show,Eq,Ord)

instance Storable Rect where
    sizeOf = const #{size SDL_Rect}
    alignment = const 2
    peek ptr
        = do x <- #{peek SDL_Rect, x} ptr :: IO CInt
             y <- #{peek SDL_Rect, y} ptr :: IO CInt
             w <- #{peek SDL_Rect, w} ptr :: IO CInt
             h <- #{peek SDL_Rect, h} ptr :: IO CInt
             return $! Rect (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)
    poke ptr (Rect x y w h)
        = do #{poke SDL_Rect, x} ptr (fromIntegral x :: CInt)
             #{poke SDL_Rect, y} ptr (fromIntegral y :: CInt)
             #{poke SDL_Rect, w} ptr (fromIntegral w :: CInt)
             #{poke SDL_Rect, h} ptr (fromIntegral h :: CInt)
