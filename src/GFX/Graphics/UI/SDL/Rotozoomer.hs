-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.SDL.Rotozoomer
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Graphics.UI.SDL.Rotozoomer where

import Foreign
import Foreign.C

import Graphics.UI.SDL.Video
import Graphics.UI.SDL.General
import Graphics.UI.SDL.Types

finalizeWhenNotNull :: String -> Ptr SurfaceStruct -> IO Surface
finalizeWhenNotNull errMsg image
    = if image == nullPtr
         then failWithError errMsg
         else mkFinalizedSurface image



-- SDL_Surface * rotozoomSurface (SDL_Surface *src, double angle, double zoom, int smooth);
foreign import ccall unsafe "rotozoomSurface" sdlRotozoom
    :: Ptr SurfaceStruct -> Double -> Double -> Int -> IO (Ptr SurfaceStruct)
rotozoom :: Surface -> Double -> Double -> Bool -> IO Surface
rotozoom src angle zoom smooth
    = withForeignPtr src $ \imgSrc ->
      sdlRotozoom imgSrc angle zoom (fromBool smooth) >>= finalizeWhenNotNull "rotozoomSurface"

{-
-- SDL_Surface * rotozoomSurfaceXY (SDL_Surface *src, double angle, double zoomx, double zoomy, int smooth);
foreign import ccall unsafe "rotozoomSurfaceXY" sdlRotozoomXY
    :: Ptr SurfaceStruct -> Double -> Double -> Double -> Int -> IO (Ptr SurfaceStruct)
rotozoomXY :: Surface -> Double -> Double -> Double -> Bool -> IO Surface
rotozoomXY src angle zoomx zoomy smooth
    = withForeignPtr src $ \imgSrc ->
      sdlRotozoomXY imgSrc angle zoomx zoomy (fromBool smooth) >>= finalizeWhenNotNull "rotozoomSurfaceXY"
-}

-- SDL_Surface * zoomSurface (SDL_Surface *src, double zoomx, double zoomy, int smooth);
foreign import ccall unsafe "zoomSurface" sdlZoom
    :: Ptr SurfaceStruct -> Double -> Double -> Int -> IO (Ptr SurfaceStruct)
zoom :: Surface -> Double -> Double -> Bool -> IO Surface
zoom src zoomx zoomy smooth
    = withForeignPtr src $ \imgSrc ->
      sdlZoom imgSrc zoomx zoomy (fromBool smooth) >>= finalizeWhenNotNull "zoomSurface"


