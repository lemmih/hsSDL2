-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.SDL.Types
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Graphics.UI.SDL.Types
    ( SurfaceStruct
    , Surface
    , VideoInfoStruct
    , VideoInfo
    , RWopsStruct
    , RWops
    , PixelFormatStruct
    , PixelFormat
    , TimerIDStruct
    , SurfaceFlag (..)
    , surfaceGetPixelFormat
    , surfaceGetWidth
    , surfaceGetHeight
    , surfaceGetFlags
    , surfaceGetPitch
    , surfaceGetPixels
    , pixelFormatGetAlpha
    , pixelFormatGetColorKey
    , pixelFormatGetBitsPerPixel
    , pixelFormatGetBytesPerPixel
    ) where

import Foreign

import Graphics.UI.SDL.Utilities
import Graphics.UI.SDL.Color


data SurfaceStruct
type Surface = ForeignPtr SurfaceStruct

data VideoInfoStruct
type VideoInfo = ForeignPtr VideoInfoStruct

data RWopsStruct
type RWops = ForeignPtr RWopsStruct

data PixelFormatStruct
type PixelFormat = ForeignPtr PixelFormatStruct

data TimerIDStruct

data PixelsData
type Pixels = Ptr PixelsData

data SurfaceFlag
    = SWSurface
    | HWSurface
    | OpenGL
    | ASyncBlit
    | OpenGLBlit
    | Resizeable
    | NoFrame
    | HWAccel
    | SrcColorKey
    | RLEAccel
    | SrcAlpha
    | PreAlloc
    | AnyFormat
    | HWPalette
    | DoubleBuf
    | Fullscreen
    deriving (Eq, Ord, Show)
instance Bounded SurfaceFlag where
      minBound = SWSurface
      maxBound = Fullscreen
instance UnsignedEnum SurfaceFlag where
      fromEnumW SWSurface = 0
      fromEnumW HWSurface = 1
      fromEnumW OpenGL    = 2
      fromEnumW ASyncBlit = 4
      fromEnumW OpenGLBlit = 10
      fromEnumW Resizeable = 16
      fromEnumW NoFrame = 32
      fromEnumW HWAccel = 256
      fromEnumW SrcColorKey = 4096
      fromEnumW RLEAccel = 16384
      fromEnumW SrcAlpha = 65536
      fromEnumW PreAlloc = 16777216
      fromEnumW AnyFormat = 268435456
      fromEnumW HWPalette = 536870912
      fromEnumW DoubleBuf = 1073741824
      fromEnumW Fullscreen = 2147483648
      toEnumW 0 = SWSurface
      toEnumW 1 = HWSurface
      toEnumW 4 = ASyncBlit
      toEnumW 2 = OpenGL
      toEnumW 10 = OpenGLBlit
      toEnumW 16 = Resizeable
      toEnumW 32 = NoFrame
      toEnumW 256 = HWAccel
      toEnumW 4096 = SrcColorKey
      toEnumW 16384 = RLEAccel
      toEnumW 65536 = SrcAlpha
      toEnumW 16777216 = PreAlloc
      toEnumW 268435456 = AnyFormat
      toEnumW 536870912 = HWPalette
      toEnumW 1073741824 = DoubleBuf
      toEnumW 2147483648 = Fullscreen
      succW SWSurface = HWSurface
      succW HWSurface = OpenGL
      succW OpenGL = ASyncBlit
      succW ASyncBlit = OpenGLBlit
      succW OpenGLBlit = Resizeable
      succW Resizeable = NoFrame
      succW NoFrame = HWAccel
      succW HWAccel = SrcColorKey
      succW SrcColorKey = RLEAccel
      succW RLEAccel = SrcAlpha
      succW SrcAlpha = PreAlloc
      succW PreAlloc = AnyFormat
      succW AnyFormat = HWPalette
      succW HWPalette = DoubleBuf
      succW DoubleBuf = Fullscreen

      predW HWSurface = SWSurface
      predW OpenGL = HWSurface
      predW ASyncBlit = OpenGL
      predW OpenGLBlit = ASyncBlit
      predW Resizeable = OpenGLBlit
      predW NoFrame = Resizeable
      predW HWAccel = NoFrame
      predW SrcColorKey = HWAccel
      predW RLEAccel = SrcColorKey
      predW SrcAlpha = RLEAccel
      predW PreAlloc = SrcAlpha
      predW AnyFormat = PreAlloc
      predW HWPalette = AnyFormat
      predW DoubleBuf = HWPalette
      predW Fullscreen = DoubleBuf

      enumFromToW x y | x > y = []
                      | x == y = [y]
                      | True = x : enumFromToW (succW x) y


#include <SDL.h>

surfaceGetPixelFormat :: Surface -> PixelFormat
surfaceGetPixelFormat surface
    = unsafePerformIO $
      withForeignPtr surface $ \ptr ->
      newForeignPtr_ =<< #{peek SDL_Surface, format} ptr

pixelFormatGetAlpha :: PixelFormat -> IO Word8
pixelFormatGetAlpha format =
    withForeignPtr format $
    #{peek SDL_PixelFormat, alpha}

pixelFormatGetColorKey :: PixelFormat -> IO Pixel
pixelFormatGetColorKey format =
    fmap Pixel $
    withForeignPtr format $
    #{peek SDL_PixelFormat, colorkey}

pixelFormatGetBitsPerPixel :: PixelFormat -> IO Word8
pixelFormatGetBitsPerPixel format
    = withForeignPtr format $
      #{peek SDL_PixelFormat, BitsPerPixel}

pixelFormatGetBytesPerPixel :: PixelFormat -> IO Word8
pixelFormatGetBytesPerPixel format
    = withForeignPtr format $
      #{peek SDL_PixelFormat, BytesPerPixel}

surfaceGetWidth :: Surface -> Int
surfaceGetWidth surface
    = unsafePerformIO $
      withForeignPtr surface $
      #{peek SDL_Surface, w}

surfaceGetHeight :: Surface -> Int
surfaceGetHeight surface
    = unsafePerformIO $
      withForeignPtr surface $
      #{peek SDL_Surface, h}

surfaceGetFlags :: Surface -> IO [SurfaceFlag]
surfaceGetFlags surface
    = withForeignPtr surface $
      fmap fromBitmaskW . #{peek SDL_Surface, flags}

surfaceGetPitch :: Surface -> Word16
surfaceGetPitch surface
    = unsafePerformIO $
      withForeignPtr surface $
      #peek SDL_Surface, pitch

surfaceGetPixels :: Surface -> IO Pixels
surfaceGetPixels surface
    = withForeignPtr surface $
      #peek SDL_Surface, pixels