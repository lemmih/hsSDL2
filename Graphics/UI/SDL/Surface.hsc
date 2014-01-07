#include "SDL.h"
module Graphics.UI.SDL.Surface
    ( createRGBSurface
    , fillRect
    , fillRects
    , freeSurface
    , loadBMP
    , lockSurface
    , setColorKey
    , unlockSurface
    ) where

import Data.Vector.Storable (Vector)
import Foreign
import Foreign.C
import Graphics.UI.SDL.Color (Color)
import Graphics.UI.SDL.Rect (Rect)
import Graphics.UI.SDL.Utilities (fatalSDLBool)
import Graphics.UI.SDL.Types (RWopsStruct, Surface, SurfaceStruct)

import qualified Data.Vector.Storable as V
import qualified Graphics.UI.SDL.RWOps as RWOps

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

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_UnlockSurface"
  sdlUnlockSurface :: Ptr SurfaceStruct -> IO ()

unlockSurface :: Surface -> IO ()
unlockSurface s = withForeignPtr s sdlUnlockSurface

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_LockSurface"
  sdlLockSurface :: Ptr SurfaceStruct -> IO ()

lockSurface :: Surface -> IO ()
lockSurface s = withForeignPtr s sdlLockSurface

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_CreateRGBSurface"
  sdlCreateRGBSurface :: #{type Uint32} -> #{type int} -> #{type int} -> #{type int}
                      -> #{type Uint32} -> #{type Uint32} -> #{type Uint32}
                      -> #{type Uint32} -> IO (Ptr SurfaceStruct)

createRGBSurface
  :: #{type int} -> #{type int} -> #{type int} -> #{type Uint32}
  -> #{type Uint32}-> #{type Uint32} -> #{type Uint32} -> IO Surface
createRGBSurface w h depth rMask gMask bMask aMask =
  sdlCreateRGBSurface 0 w h depth rMask gMask bMask aMask
    >>= mkFinalizedSurface

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_LoadBMP_RW"
  sdlLoadBMP :: Ptr RWopsStruct -> CInt -> IO (Ptr SurfaceStruct)

-- TODO Decide if this should be partial or return Maybe/Either
loadBMP :: FilePath -> IO Surface
loadBMP path =
  RWOps.withFile path "r" $ \rwops ->
  withForeignPtr rwops $ \crwops -> do
    bmp <- sdlLoadBMP crwops 0
    if bmp == nullPtr
      then error "loadBMP: failed to load BMP"
      else mkFinalizedSurface bmp

--------------------------------------------------------------------------------
foreign import ccall unsafe "&SDL_FreeSurface"
  sdlFreeSurface_finalizer :: FunPtr (Ptr SurfaceStruct -> IO ())

freeSurface :: Surface -> IO ()
freeSurface = finalizeForeignPtr

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_SetColorKey"
  sdlSetColorKey :: Ptr SurfaceStruct -> CInt -> Word32 -> IO #{type int}

setColorKey :: Surface -> Bool -> Word32 -> IO ()
setColorKey s enabled pixel = withForeignPtr s $ \cs ->
  fatalSDLBool "SDL_SetColorKey" $
    sdlSetColorKey cs (if enabled then 1 else 0) pixel

--------------------------------------------------------------------------------
mkFinalizedSurface :: Ptr SurfaceStruct -> IO Surface
mkFinalizedSurface = newForeignPtr sdlFreeSurface_finalizer
