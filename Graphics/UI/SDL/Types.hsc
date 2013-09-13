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


