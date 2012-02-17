#include "SDL.h"
#ifdef main
#undef main
#endif
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
    , JoystickStruct
    , Joystick
    , Hat(..)
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
    , videoInfoWidth
    , videoInfoHeight
    ) where

import Foreign.C (CInt)
import Foreign (Word8, Word16, Word32, Ptr, Storable(peekByteOff),
                newForeignPtr_, ForeignPtr, withForeignPtr)
import System.IO.Unsafe (unsafePerformIO)

import Graphics.UI.SDL.Utilities (Enum(..), fromBitmask)
import Graphics.UI.SDL.Color (Pixel(..))

import Prelude hiding (Enum(..))

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

data JoystickStruct
type Joystick = ForeignPtr JoystickStruct

data Hat
    = HatCentered
    | HatUp
    | HatRight
    | HatDown
    | HatLeft
    | HatRightUp
    | HatRightDown
    | HatLeftUp
    | HatLeftDown
      deriving (Show,Eq,Ord)

instance Bounded Hat where
    minBound = HatCentered
    maxBound = HatLeftDown

instance Enum Hat Word8 where
    fromEnum HatCentered = #{const SDL_HAT_CENTERED}
    fromEnum HatUp = #{const SDL_HAT_UP}
    fromEnum HatRight = #{const SDL_HAT_RIGHT}
    fromEnum HatDown = #{const SDL_HAT_DOWN}
    fromEnum HatLeft = #{const SDL_HAT_LEFT}
    fromEnum HatRightUp = #{const SDL_HAT_RIGHTUP}
    fromEnum HatRightDown = #{const SDL_HAT_RIGHTDOWN}
    fromEnum HatLeftUp = #{const SDL_HAT_LEFTUP}
    fromEnum HatLeftDown = #{const SDL_HAT_LEFTDOWN}
    toEnum #{const SDL_HAT_CENTERED} = HatCentered
    toEnum #{const SDL_HAT_UP} = HatUp
    toEnum #{const SDL_HAT_RIGHT} = HatRight
    toEnum #{const SDL_HAT_DOWN} = HatDown
    toEnum #{const SDL_HAT_LEFT} = HatLeft
    toEnum #{const SDL_HAT_RIGHTUP} = HatRightUp
    toEnum #{const SDL_HAT_RIGHTDOWN} = HatRightDown
    toEnum #{const SDL_HAT_LEFTUP} = HatLeftUp
    toEnum #{const SDL_HAT_LEFTDOWN} = HatLeftDown
    toEnum _ = error "Graphics.UI.SDL.Types.toEnum: bad argument"
    succ HatCentered = HatUp
    succ HatUp = HatRight
    succ HatRight = HatDown
    succ HatDown = HatLeft
    succ HatLeft = HatRightUp
    succ HatRightUp = HatRightDown
    succ HatRightDown = HatLeftUp
    succ HatLeftUp = HatLeftDown
    succ _ = error "Graphics.UI.SDL.Types.succ: bad argument"
    pred HatUp = HatCentered
    pred HatRight = HatUp
    pred HatDown = HatRight
    pred HatLeft = HatDown
    pred HatRightUp = HatLeft
    pred HatRightDown = HatRightUp
    pred HatLeftUp = HatRightDown
    pred HatLeftDown = HatLeftUp
    pred _ = error "Graphics.UI.SDL.Types.pred: bad argument"
    enumFromTo x y | x > y = []
                   | x == y = [y]
                   | True = x : enumFromTo (succ x) y
    

data SurfaceFlag
    = SWSurface
    | HWSurface
    | OpenGL
    | ASyncBlit
    | OpenGLBlit
    | Resizable
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
    deriving (Eq, Ord, Show, Read)
instance Bounded SurfaceFlag where
      minBound = SWSurface
      maxBound = Fullscreen
instance Enum SurfaceFlag Word32 where
      fromEnum SWSurface = 0
      fromEnum HWSurface = 1
      fromEnum OpenGL    = 2
      fromEnum ASyncBlit = 4
      fromEnum OpenGLBlit = 10
      fromEnum Resizable = 16
      fromEnum NoFrame = 32
      fromEnum HWAccel = 256
      fromEnum SrcColorKey = 4096
      fromEnum RLEAccel = 16384
      fromEnum SrcAlpha = 65536
      fromEnum PreAlloc = 16777216
      fromEnum AnyFormat = 268435456
      fromEnum HWPalette = 536870912
      fromEnum DoubleBuf = 1073741824
      fromEnum Fullscreen = 2147483648
      toEnum 0 = SWSurface
      toEnum 1 = HWSurface
      toEnum 4 = ASyncBlit
      toEnum 2 = OpenGL
      toEnum 10 = OpenGLBlit
      toEnum 16 = Resizable
      toEnum 32 = NoFrame
      toEnum 256 = HWAccel
      toEnum 4096 = SrcColorKey
      toEnum 16384 = RLEAccel
      toEnum 65536 = SrcAlpha
      toEnum 16777216 = PreAlloc
      toEnum 268435456 = AnyFormat
      toEnum 536870912 = HWPalette
      toEnum 1073741824 = DoubleBuf
      toEnum 2147483648 = Fullscreen
      toEnum _ = error "Graphics.UI.SDL.Types.fromEnum: bad argument"
      succ SWSurface = HWSurface
      succ HWSurface = OpenGL
      succ OpenGL = ASyncBlit
      succ ASyncBlit = OpenGLBlit
      succ OpenGLBlit = Resizable
      succ Resizable = NoFrame
      succ NoFrame = HWAccel
      succ HWAccel = SrcColorKey
      succ SrcColorKey = RLEAccel
      succ RLEAccel = SrcAlpha
      succ SrcAlpha = PreAlloc
      succ PreAlloc = AnyFormat
      succ AnyFormat = HWPalette
      succ HWPalette = DoubleBuf
      succ DoubleBuf = Fullscreen
      succ _ = error "Graphics.UI.SDL.Types.succ: bad argument"

      pred HWSurface = SWSurface
      pred OpenGL = HWSurface
      pred ASyncBlit = OpenGL
      pred OpenGLBlit = ASyncBlit
      pred Resizable = OpenGLBlit
      pred NoFrame = Resizable
      pred HWAccel = NoFrame
      pred SrcColorKey = HWAccel
      pred RLEAccel = SrcColorKey
      pred SrcAlpha = RLEAccel
      pred PreAlloc = SrcAlpha
      pred AnyFormat = PreAlloc
      pred HWPalette = AnyFormat
      pred DoubleBuf = HWPalette
      pred Fullscreen = DoubleBuf
      pred _ = error "Graphics.UI.SDL.Types.pred: bad argument"

      enumFromTo x y | x > y = []
                     | x == y = [y]
                     | True = x : enumFromTo (succ x) y


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

cintToInt :: CInt -> Int
cintToInt = fromIntegral

surfaceGetWidth :: Surface -> Int
surfaceGetWidth surface
    = cintToInt $ unsafePerformIO $
      withForeignPtr surface $
      #{peek SDL_Surface, w}

surfaceGetHeight :: Surface -> Int
surfaceGetHeight surface
    = cintToInt $ unsafePerformIO $
      withForeignPtr surface $
      #{peek SDL_Surface, h}

surfaceGetFlags :: Surface -> IO [SurfaceFlag]
surfaceGetFlags surface
    = withForeignPtr surface $
      fmap fromBitmask . #{peek SDL_Surface, flags}

surfaceGetPitch :: Surface -> Word16
surfaceGetPitch surface
    = unsafePerformIO $
      withForeignPtr surface $
      #peek SDL_Surface, pitch

surfaceGetPixels :: Surface -> IO Pixels
surfaceGetPixels surface
    = withForeignPtr surface $
      #peek SDL_Surface, pixels

videoInfoWidth :: VideoInfo -> Int
videoInfoWidth vi
    = cintToInt $ unsafePerformIO $
      withForeignPtr vi $
      #peek SDL_VideoInfo, current_w

videoInfoHeight :: VideoInfo -> Int
videoInfoHeight vi
    = cintToInt $ unsafePerformIO $
      withForeignPtr vi $
      #peek SDL_VideoInfo, current_h

