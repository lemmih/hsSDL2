-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.SDL.Color
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Graphics.UI.SDL.Color where

import Data.Word
import Foreign
import Foreign.Storable

data Color
    = Color
    { colorRed, colorGreen, colorBlue :: Word8 }

instance Storable Color where
    sizeOf = const 4
    alignment = const 1
    peek ptr
        = do r <- peekByteOff ptr 0
             g <- peekByteOff ptr 1
             b <- peekByteOff ptr 2
             return (Color r g b)
    poke ptr (Color r g b) = pokeArray (castPtr ptr) [r,g,b,0]

newtype Pixel = Pixel Word32 deriving (Show,Eq,Ord)

