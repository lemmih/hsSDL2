#include "SDL.h"
module Graphics.UI.SDL.Surface
    ( blitScaled
    , blitSurface
    , createRGBSurface
    , fillRect
    , fillRects
    , freeSurface
    , getSurfaceAlphaMod
    , loadBMP
    , lockSurface
    , setColorKey
    , setSurfaceAlphaMod
    , unlockSurface
    ) where

import Data.Vector.Storable (Vector)
import Foreign
import Foreign.C
import Graphics.UI.SDL.Color (Color)
import Graphics.UI.SDL.Rect (Rect)
import Graphics.UI.SDL.Utilities (fatalSDLBool)
import Graphics.UI.SDL.Types (RWopsStruct, Surface, SurfaceStruct)
import Graphics.UI.SDL.Raw

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
foreign import ccall unsafe "SDL_SetSurfaceAlphaMod"
  sdlSetSurfaceAlphaMod :: Ptr SurfaceStruct -> #{type Uint8} -> IO #{type int}

setSurfaceAlphaMod :: Surface -> #{type Uint8} -> IO ()
setSurfaceAlphaMod s alphaMod = withForeignPtr s $ \cS ->
  fatalSDLBool "SDL_SetSurfaceAlphaMod" $ sdlSetSurfaceAlphaMod cS alphaMod

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_GetSurfaceAlphaMod"
  sdlGetSurfaceAlphaMod :: Ptr SurfaceStruct -> Ptr #{type Uint8} -> IO #{type int}

getSurfaceAlphaMod :: Surface -> IO #{type Uint8}
getSurfaceAlphaMod s =
  withForeignPtr s $ \cS ->
  alloca $ \alphaModPtr -> do
    fatalSDLBool "SDL_GetSurfaceAlphaMod" $ sdlGetSurfaceAlphaMod cS alphaModPtr
    peek alphaModPtr

--------------------------------------------------------------------------------
type SDLBlitF = Ptr SurfaceStruct -> Ptr Rect -> Ptr SurfaceStruct -> Ptr Rect -> IO #{type int}

foreign import ccall unsafe "SDL_UpperBlit"
  sdlUpperBlit :: SDLBlitF

foreign import ccall unsafe "SDL_UpperBlitScaled"
  sdlUpperBlitScaled :: SDLBlitF

-- mirror the defines in SDL_surface.h
sdlBlitSurface = sdlUpperBlit
sdlBlitScaled = sdlUpperBlitScaled

blitSurface :: Surface -> Maybe Rect -> Surface -> Maybe Rect -> IO ()
blitSurface = doBlit "SDL_BlitSurface" sdlBlitSurface

blitScaled :: Surface -> Maybe Rect -> Surface -> Maybe Rect -> IO ()
blitScaled = doBlit "SDL_BlitScaled" sdlBlitScaled

doBlit :: String -> SDLBlitF -> Surface -> Maybe Rect -> Surface -> Maybe Rect -> IO ()
doBlit name f srcSurface srcRect dstSurface dstRect =
  withForeignPtr srcSurface $ \srcSurfacePtr ->
  withForeignPtr dstSurface $ \dstSurfacePtr ->
  maybeWith with srcRect $ \srcRectPtr ->
  maybeWith with dstRect $ \dstRectPtr ->
  fatalSDLBool name $
    f srcSurfacePtr srcRectPtr dstSurfacePtr dstRectPtr
