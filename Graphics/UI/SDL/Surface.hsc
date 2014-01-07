#include "SDL.h"
module Graphics.UI.SDL.Surface
    ( fillRect
    , fillRects
    ) where

import Data.Vector.Storable (Vector)
import Foreign
import Graphics.UI.SDL.Color (Color)
import Graphics.UI.SDL.Rect (Rect)
import Graphics.UI.SDL.Utilities (fatalSDLBool)
import Graphics.UI.SDL.Types (Surface, SurfaceStruct)

import qualified Data.Vector.Storable as V

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_FillRect"
  sdlFillRect :: Ptr SurfaceStruct -> Ptr Rect -> #{type Uint32} -> IO #{type int}

fillRect :: Surface -> Rect -> Color -> IO ()
fillRect s r color =
  withForeignPtr s $ \cS ->
  with r $ \cR ->
    colorToInt color >>= fatalSDLBool "SDL_FillRect" . sdlFillRect cS cR

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_FillRects"
  sdlFillRects :: Ptr SurfaceStruct -> Ptr Rect -> #{type int} -> #{type Uint32} -> IO #{type int}

fillRects :: Surface -> Vector Rect -> Color -> IO ()
fillRects s rects color =
  withForeignPtr s $ \cS ->
  V.unsafeWith rects $ \cR ->
    colorToInt color >>=
      fatalSDLBool "SDL_FillRect" . sdlFillRects cS cR (fromIntegral $ V.length rects)

--------------------------------------------------------------------------------
colorToInt :: Color -> IO #{type Uint32}
colorToInt color = alloca $ \colorPtr ->
  poke (castPtr colorPtr) color >> peek colorPtr
