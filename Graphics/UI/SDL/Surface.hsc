#include "SDL.h"
module Graphics.UI.SDL.Surface
    ( fillRect
    ) where

import Foreign
import Graphics.UI.SDL.Color (Color)
import Graphics.UI.SDL.Rect (Rect)
import Graphics.UI.SDL.Utilities (fatalSDLBool)
import Graphics.UI.SDL.Types (Surface, SurfaceStruct)

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_FillRect"
  sdlFillRect :: Ptr SurfaceStruct -> Ptr Rect -> #{type Uint32} -> IO #{type int}

fillRect :: Surface -> Rect -> Color -> IO ()
fillRect s r color =
  withForeignPtr s $ \cS ->
  alloca $ \colorPtr ->
  with r $ \cR -> do
    poke (castPtr colorPtr) color
    peek colorPtr >>= fatalSDLBool "SDL_FillRect" . sdlFillRect cS cR
