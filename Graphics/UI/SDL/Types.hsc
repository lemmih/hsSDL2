#include "SDL.h"
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
  ( SDL_bool
  , WindowStruct, Window
  , RendererStruct, Renderer
  , GLContextStruct, GLContext
  , SurfaceStruct, Surface
  , RWopsStruct, RWops
  , TextureStruct, Texture
  , Size(..), mkSize
  , Position(..), mkPosition
  , WindowFlag(..)
  , windowFlagToC
  , RenderingDevice(..)
  , RendererFlag(..)
  , rendererFlagToC
  , PixelFormatStruct
  , PixelFormat
  , CursorStruct
  , Cursor
  , PixelFormatEnum(..)
  , pixelFormatEnumFromC
  , pixelFormatEnumToC
  , TextureAccess(..)
  , textureAccessToC
  ) where

import Foreign.C (CInt, CUInt)
import Foreign
--import System.IO.Unsafe (unsafePerformIO)

--import Graphics.UI.SDL.Utilities (Enum(..), fromBitmask)
--import Graphics.UI.SDL.Color (Pixel(..))

type SDL_bool = #{type SDL_bool}

data WindowStruct
type Window = ForeignPtr WindowStruct

data RendererStruct
type Renderer = ForeignPtr RendererStruct

data SurfaceStruct
type Surface = ForeignPtr SurfaceStruct

data RWopsStruct
type RWops = ForeignPtr RWopsStruct

data TextureStruct
type Texture = ForeignPtr TextureStruct

data GLContextStruct
type GLContext = ForeignPtr GLContextStruct

data PixelFormatStruct
type PixelFormat = ForeignPtr PixelFormatStruct

data CursorStruct
type Cursor = ForeignPtr CursorStruct

data Size = Size { sizeWidth :: Int, sizeHeight :: Int }
  deriving ( Read, Show, Eq, Ord )

-- smart constructor
mkSize :: CInt -> CInt -> Size
mkSize width height = Size (fromIntegral width) (fromIntegral height)

data Position = Position { positionX :: Int, positionY :: Int }
  deriving ( Read, Show, Eq, Ord )

-- smart constructor
mkPosition :: CInt -> CInt -> Position
mkPosition x y = Position (fromIntegral x) (fromIntegral y)

data PixelFormatEnum
  = PixelFormatUnknown
  | PixelFormatIndex1LSB
  | PixelFormatIndex1MSB
  | PixelFormatIndex4LSB
  | PixelFormatIndex4MSB
  | PixelFormatIndex8
  | PixelFormatRGB332
  | PixelFormatRGB444
  | PixelFormatRGB555
  | PixelFormatBGR555
  | PixelFormatARGB4444
  | PixelFormatRGBA4444
  | PixelFormatABGR4444
  | PixelFormatBGRA4444
  | PixelFormatARGB1555
  | PixelFormatRGBA5551
  | PixelFormatBGRA5551
  | PixelFormatRGB565
  | PixelFormatBGR565
  | PixelFormatRGB24
  | PixelFormatBGR24
  | PixelFormatRGB888
  | PixelFormatRGBX8888
  | PixelFormatBGR888
  | PixelFormatBGRX8888
  | PixelFormatARGB8888
  | PixelFormatRGBA8888
  | PixelFormatABGR8888
  | PixelFormatBGRA8888
  | PixelFormatARGB2101010
  | PixelFormatYV12
  | PixelFormatIYUV
  | PixelFormatYUY2
  | PixelFormatUYVY
  | PixelFormatYVYU
  deriving (Eq, Show)

pixelFormatEnumFromC :: Word32 -> PixelFormatEnum
pixelFormatEnumFromC #{const SDL_PIXELFORMAT_UNKNOWN} = PixelFormatUnknown
pixelFormatEnumFromC #{const SDL_PIXELFORMAT_INDEX1LSB} = PixelFormatIndex1LSB
pixelFormatEnumFromC #{const SDL_PIXELFORMAT_INDEX1MSB} = PixelFormatIndex1MSB
pixelFormatEnumFromC #{const SDL_PIXELFORMAT_INDEX4LSB} = PixelFormatIndex4LSB
pixelFormatEnumFromC #{const SDL_PIXELFORMAT_INDEX4MSB} = PixelFormatIndex4MSB
pixelFormatEnumFromC #{const SDL_PIXELFORMAT_INDEX8} = PixelFormatIndex8
pixelFormatEnumFromC #{const SDL_PIXELFORMAT_RGB332} = PixelFormatRGB332
pixelFormatEnumFromC #{const SDL_PIXELFORMAT_RGB444} = PixelFormatRGB444
pixelFormatEnumFromC #{const SDL_PIXELFORMAT_RGB555} = PixelFormatRGB555
pixelFormatEnumFromC #{const SDL_PIXELFORMAT_BGR555} = PixelFormatBGR555
pixelFormatEnumFromC #{const SDL_PIXELFORMAT_ARGB4444} = PixelFormatARGB4444
pixelFormatEnumFromC #{const SDL_PIXELFORMAT_RGBA4444} = PixelFormatRGBA4444
pixelFormatEnumFromC #{const SDL_PIXELFORMAT_ABGR4444} = PixelFormatABGR4444
pixelFormatEnumFromC #{const SDL_PIXELFORMAT_BGRA4444} = PixelFormatBGRA4444
pixelFormatEnumFromC #{const SDL_PIXELFORMAT_ARGB1555} = PixelFormatARGB1555
pixelFormatEnumFromC #{const SDL_PIXELFORMAT_RGBA5551} = PixelFormatRGBA5551
pixelFormatEnumFromC #{const SDL_PIXELFORMAT_ABGR1555} = PixelFormatBGRA5551
pixelFormatEnumFromC #{const SDL_PIXELFORMAT_RGB565} = PixelFormatRGB565
pixelFormatEnumFromC #{const SDL_PIXELFORMAT_BGR565} = PixelFormatBGR565
pixelFormatEnumFromC #{const SDL_PIXELFORMAT_RGB24} = PixelFormatRGB24
pixelFormatEnumFromC #{const SDL_PIXELFORMAT_BGR24} = PixelFormatBGR24
pixelFormatEnumFromC #{const SDL_PIXELFORMAT_RGB888} = PixelFormatRGB888
pixelFormatEnumFromC #{const SDL_PIXELFORMAT_RGBX8888} = PixelFormatRGBX8888
pixelFormatEnumFromC #{const SDL_PIXELFORMAT_BGR888} = PixelFormatBGR888
pixelFormatEnumFromC #{const SDL_PIXELFORMAT_BGRX8888} = PixelFormatBGRX8888
pixelFormatEnumFromC #{const SDL_PIXELFORMAT_ARGB8888} = PixelFormatARGB8888
pixelFormatEnumFromC #{const SDL_PIXELFORMAT_RGBA8888} = PixelFormatRGBA8888
pixelFormatEnumFromC #{const SDL_PIXELFORMAT_ABGR8888} = PixelFormatABGR8888
pixelFormatEnumFromC #{const SDL_PIXELFORMAT_BGRA8888} = PixelFormatBGRA8888
pixelFormatEnumFromC #{const SDL_PIXELFORMAT_ARGB2101010} = PixelFormatARGB2101010
pixelFormatEnumFromC #{const SDL_PIXELFORMAT_YV12} = PixelFormatYV12
pixelFormatEnumFromC #{const SDL_PIXELFORMAT_IYUV} = PixelFormatIYUV
pixelFormatEnumFromC #{const SDL_PIXELFORMAT_YUY2} = PixelFormatYUY2
pixelFormatEnumFromC #{const SDL_PIXELFORMAT_UYVY} = PixelFormatUYVY
pixelFormatEnumFromC #{const SDL_PIXELFORMAT_YVYU} = PixelFormatYVYU
pixelFormatEnumFromC unknown = error $ "Graphics.UI.SDL.Types.pixelFormatEnumFromC: unknown PixelFormat: " ++ show unknown

pixelFormatEnumToC :: PixelFormatEnum -> Word32
pixelFormatEnumToC PixelFormatUnknown = #{const SDL_PIXELFORMAT_UNKNOWN}
pixelFormatEnumToC PixelFormatIndex1LSB = #{const SDL_PIXELFORMAT_INDEX1LSB}
pixelFormatEnumToC PixelFormatIndex1MSB = #{const SDL_PIXELFORMAT_INDEX1MSB}
pixelFormatEnumToC PixelFormatIndex4LSB = #{const SDL_PIXELFORMAT_INDEX4LSB}
pixelFormatEnumToC PixelFormatIndex4MSB = #{const SDL_PIXELFORMAT_INDEX4MSB}
pixelFormatEnumToC PixelFormatIndex8 = #{const SDL_PIXELFORMAT_INDEX8}
pixelFormatEnumToC PixelFormatRGB332 = #{const SDL_PIXELFORMAT_RGB332}
pixelFormatEnumToC PixelFormatRGB444 = #{const SDL_PIXELFORMAT_RGB444}
pixelFormatEnumToC PixelFormatRGB555 = #{const SDL_PIXELFORMAT_RGB555}
pixelFormatEnumToC PixelFormatBGR555 = #{const SDL_PIXELFORMAT_BGR555}
pixelFormatEnumToC PixelFormatARGB4444 = #{const SDL_PIXELFORMAT_ARGB4444}
pixelFormatEnumToC PixelFormatRGBA4444 = #{const SDL_PIXELFORMAT_RGBA4444}
pixelFormatEnumToC PixelFormatABGR4444 = #{const SDL_PIXELFORMAT_ABGR4444}
pixelFormatEnumToC PixelFormatBGRA4444 = #{const SDL_PIXELFORMAT_BGRA4444}
pixelFormatEnumToC PixelFormatARGB1555 = #{const SDL_PIXELFORMAT_ARGB1555}
pixelFormatEnumToC PixelFormatRGBA5551 = #{const SDL_PIXELFORMAT_RGBA5551}
pixelFormatEnumToC PixelFormatBGRA5551 = #{const SDL_PIXELFORMAT_ABGR1555}
pixelFormatEnumToC PixelFormatRGB565 = #{const SDL_PIXELFORMAT_RGB565}
pixelFormatEnumToC PixelFormatBGR565 = #{const SDL_PIXELFORMAT_BGR565}
pixelFormatEnumToC PixelFormatRGB24 = #{const SDL_PIXELFORMAT_RGB24}
pixelFormatEnumToC PixelFormatBGR24 = #{const SDL_PIXELFORMAT_BGR24}
pixelFormatEnumToC PixelFormatRGB888 = #{const SDL_PIXELFORMAT_RGB888}
pixelFormatEnumToC PixelFormatRGBX8888 = #{const SDL_PIXELFORMAT_RGBX8888}
pixelFormatEnumToC PixelFormatBGR888 = #{const SDL_PIXELFORMAT_BGR888}
pixelFormatEnumToC PixelFormatBGRX8888 = #{const SDL_PIXELFORMAT_BGRX8888}
pixelFormatEnumToC PixelFormatARGB8888 = #{const SDL_PIXELFORMAT_ARGB8888}
pixelFormatEnumToC PixelFormatRGBA8888 = #{const SDL_PIXELFORMAT_RGBA8888}
pixelFormatEnumToC PixelFormatABGR8888 = #{const SDL_PIXELFORMAT_ABGR8888}
pixelFormatEnumToC PixelFormatBGRA8888 = #{const SDL_PIXELFORMAT_BGRA8888}
pixelFormatEnumToC PixelFormatARGB2101010 = #{const SDL_PIXELFORMAT_ARGB2101010}
pixelFormatEnumToC PixelFormatYV12 = #{const SDL_PIXELFORMAT_YV12}
pixelFormatEnumToC PixelFormatIYUV = #{const SDL_PIXELFORMAT_IYUV}
pixelFormatEnumToC PixelFormatYUY2 = #{const SDL_PIXELFORMAT_YUY2}
pixelFormatEnumToC PixelFormatUYVY = #{const SDL_PIXELFORMAT_UYVY}
pixelFormatEnumToC PixelFormatYVYU = #{const SDL_PIXELFORMAT_YVYU}

data TextureAccess
  = TextureAccessStatic
  | TextureAccessStreaming
  | TextureAccessTarget

textureAccessToC :: TextureAccess -> CInt
textureAccessToC TextureAccessStatic = #{const SDL_TEXTUREACCESS_STATIC}
textureAccessToC TextureAccessStreaming = #{const SDL_TEXTUREACCESS_STREAMING}
textureAccessToC TextureAccessTarget = #{const SDL_TEXTUREACCESS_TARGET}

data WindowFlag
  = WindowFullscreen         -- ^ fullscreen window
  | WindowFullscreenDesktop  -- ^ fullscreen window at the current desktop resolution
  | WindowOpengl             -- ^ window usable with OpenGL context
  | WindowShown              -- ^ window is visible
  | WindowHidden             -- ^ window is not visible
  | WindowBorderless         -- ^ no window decoration
  | WindowResizable          -- ^ window can be resized
  | WindowMinimized          -- ^ window is minimized
  | WindowMaximized          -- ^ window is maximized
  | WindowInputGrabbed       -- ^ window has grabbed input focus
  | WindowInputFocus         -- ^ window has input focus
  | WindowMouseFocus         -- ^ window has mouse focus
  | WindowForeign            -- ^ window not created by SDL
    deriving ( Eq, Ord, Read, Show, Bounded, Enum )

windowFlagToC :: WindowFlag -> #{type Uint32}
windowFlagToC WindowFullscreen        = #{const SDL_WINDOW_FULLSCREEN}
windowFlagToC WindowFullscreenDesktop = #{const SDL_WINDOW_FULLSCREEN_DESKTOP}
windowFlagToC WindowOpengl            = #{const SDL_WINDOW_OPENGL}
windowFlagToC WindowShown             = #{const SDL_WINDOW_SHOWN}
windowFlagToC WindowHidden            = #{const SDL_WINDOW_HIDDEN}
windowFlagToC WindowBorderless        = #{const SDL_WINDOW_BORDERLESS}
windowFlagToC WindowResizable         = #{const SDL_WINDOW_RESIZABLE}
windowFlagToC WindowMinimized         = #{const SDL_WINDOW_MINIMIZED}
windowFlagToC WindowMaximized         = #{const SDL_WINDOW_MAXIMIZED}
windowFlagToC WindowInputGrabbed      = #{const SDL_WINDOW_INPUT_GRABBED}
windowFlagToC WindowInputFocus        = #{const SDL_WINDOW_INPUT_FOCUS}
windowFlagToC WindowMouseFocus        = #{const SDL_WINDOW_MOUSE_FOCUS}
windowFlagToC WindowForeign           = #{const SDL_WINDOW_FOREIGN}

data RenderingDevice = Device Int | FirstSupported
  deriving ( Eq, Ord, Read, Show )

data RendererFlag = Software | Accelerated | PresentVSync | TargetTexture
  deriving ( Eq, Ord, Read, Show, Bounded, Enum )

rendererFlagToC :: RendererFlag -> CUInt
rendererFlagToC Software = #{const SDL_RENDERER_SOFTWARE}
rendererFlagToC Accelerated = #{const SDL_RENDERER_ACCELERATED}
rendererFlagToC PresentVSync = #{const SDL_RENDERER_PRESENTVSYNC}
rendererFlagToC TargetTexture = #{const SDL_RENDERER_TARGETTEXTURE}

