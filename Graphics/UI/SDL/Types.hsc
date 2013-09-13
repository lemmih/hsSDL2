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
  , SurfaceStruct, Surface
  , RWopsStruct, RWops
  , TextureStruct, Texture
  , Size(..)
  , Position(..)
  , WindowFlag(..)
  , windowFlagToC
  , RenderingDevice(..)
  , RendererFlag(..)
  , rendererFlagToC
  ) where

import Foreign.C (CUInt)
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

data Size = Size { sizeWidth :: Int, sizeHeight :: Int }
  deriving ( Read, Show, Eq, Ord )

data Position = Position { positionX :: Int, positionY :: Int }
  deriving ( Read, Show, Eq, Ord )

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

windowFlagToC :: WindowFlag -> CUInt
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

rendererFlagToC Software = #{const SDL_RENDERER_SOFTWARE}
rendererFlagToC Accelerated = #{const SDL_RENDERER_ACCELERATED}
rendererFlagToC PresentVSync = #{const SDL_RENDERER_PRESENTVSYNC}
rendererFlagToC TargetTexture = #{const SDL_RENDERER_TARGETTEXTURE}


