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
  , RenderingDevice(..)
  , RendererFlag(..)
  ) where

--import Foreign.C (CInt)
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
    deriving ( Eq, Ord, Read, Show )

instance Enum WindowFlag where
  fromEnum WindowFullscreen        = #{const SDL_WINDOW_FULLSCREEN}
  fromEnum WindowFullscreenDesktop = #{const SDL_WINDOW_FULLSCREEN_DESKTOP}
  fromEnum WindowOpengl            = #{const SDL_WINDOW_OPENGL}
  fromEnum WindowShown             = #{const SDL_WINDOW_SHOWN}
  fromEnum WindowHidden            = #{const SDL_WINDOW_HIDDEN}
  fromEnum WindowBorderless        = #{const SDL_WINDOW_BORDERLESS}
  fromEnum WindowResizable         = #{const SDL_WINDOW_RESIZABLE}
  fromEnum WindowMinimized         = #{const SDL_WINDOW_MINIMIZED}
  fromEnum WindowMaximized         = #{const SDL_WINDOW_MAXIMIZED}
  fromEnum WindowInputGrabbed      = #{const SDL_WINDOW_INPUT_GRABBED}
  fromEnum WindowInputFocus        = #{const SDL_WINDOW_INPUT_FOCUS}
  fromEnum WindowMouseFocus        = #{const SDL_WINDOW_MOUSE_FOCUS}
  fromEnum WindowForeign           = #{const SDL_WINDOW_FOREIGN}

  toEnum #{const SDL_WINDOW_FULLSCREEN}         = WindowFullscreen
  toEnum #{const SDL_WINDOW_FULLSCREEN_DESKTOP} = WindowFullscreenDesktop
  toEnum #{const SDL_WINDOW_OPENGL}             = WindowOpengl
  toEnum #{const SDL_WINDOW_SHOWN}              = WindowShown
  toEnum #{const SDL_WINDOW_HIDDEN}             = WindowHidden
  toEnum #{const SDL_WINDOW_BORDERLESS}         = WindowBorderless
  toEnum #{const SDL_WINDOW_RESIZABLE}          = WindowResizable
  toEnum #{const SDL_WINDOW_MINIMIZED}          = WindowMinimized
  toEnum #{const SDL_WINDOW_MAXIMIZED}          = WindowMaximized
  toEnum #{const SDL_WINDOW_INPUT_GRABBED}      = WindowInputGrabbed
  toEnum #{const SDL_WINDOW_INPUT_FOCUS}        = WindowInputFocus
  toEnum #{const SDL_WINDOW_MOUSE_FOCUS}        = WindowMouseFocus
  toEnum #{const SDL_WINDOW_FOREIGN}            = WindowForeign
  toEnum _ = error "Graphics.UI.SDL.Video.toEnum (WindowFlag): bad argument"

  succ WindowFullscreen        = WindowFullscreenDesktop
  succ WindowFullscreenDesktop = WindowOpengl
  succ WindowOpengl            = WindowShown
  succ WindowShown             = WindowHidden
  succ WindowHidden            = WindowBorderless
  succ WindowBorderless        = WindowResizable
  succ WindowResizable         = WindowMinimized
  succ WindowMinimized         = WindowMaximized
  succ WindowMaximized         = WindowInputGrabbed
  succ WindowInputGrabbed      = WindowInputFocus
  succ WindowInputFocus        = WindowMouseFocus
  succ WindowMouseFocus        = WindowForeign
  succ WindowForeign           = error "Graphics.UI.SDL.Video.succ (WindowFlag): bad argument"

  pred WindowFullscreen        = error "Graphics.UI.SDL.Video.pred (WindowFlag): bad argument"
  pred WindowFullscreenDesktop = WindowFullscreen
  pred WindowOpengl            = WindowFullscreenDesktop
  pred WindowShown             = WindowOpengl
  pred WindowHidden            = WindowShown
  pred WindowBorderless        = WindowHidden
  pred WindowResizable         = WindowBorderless
  pred WindowMinimized         = WindowResizable
  pred WindowMaximized         = WindowMinimized
  pred WindowInputGrabbed      = WindowMaximized
  pred WindowInputFocus        = WindowInputGrabbed
  pred WindowMouseFocus        = WindowInputFocus
  pred WindowForeign           = WindowMouseFocus

data RenderingDevice = Device Int | FirstSupported

data RendererFlag = Software | Accelerated | PresentVSync | TargetTexture

instance Enum RendererFlag where
  fromEnum Software = #{const SDL_RENDERER_SOFTWARE}
  fromEnum Accelerated = #{const SDL_RENDERER_ACCELERATED}
  fromEnum PresentVSync = #{const SDL_RENDERER_PRESENTVSYNC}
  fromEnum TargetTexture = #{const SDL_RENDERER_TARGETTEXTURE}

  toEnum #{const SDL_RENDERER_SOFTWARE} = Software
  toEnum #{const SDL_RENDERER_ACCELERATED} = Accelerated
  toEnum #{const SDL_RENDERER_PRESENTVSYNC} = PresentVSync
  toEnum #{const SDL_RENDERER_TARGETTEXTURE} = TargetTexture
  toEnum _ = error "Graphics.UI.SDL.Video.toEnum (RendererFlag): bad argument"

  succ Software = Accelerated
  succ Accelerated = PresentVSync
  succ PresentVSync = TargetTexture
  succ _ = error "Graphics.UI.SDL.Video.succ (RendererFlag): bad argument"

  pred Accelerated = Software
  pred PresentVSync = Accelerated
  pred TargetTexture = PresentVSync
  pred _ = error "Graphics.UI.SDL.Video.pred (RendererFlag): bad argument"


