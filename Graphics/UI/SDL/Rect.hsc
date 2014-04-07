#include "SDL.h"
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.SDL.Video
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Graphics.UI.SDL.Rect
    ( Rect(..)
    , Point(..)
    , enclosePoints
    , rectEquals
    , rectEmpty
    , hasIntersection
    , intersectRect
    , intersectRectAndLine
    , unionRect
    ) where

import Foreign
import Foreign.C
import Graphics.UI.SDL.Types
import Graphics.UI.SDL.General
import Graphics.UI.SDL.Utilities

foreign import ccall unsafe "SDL_EnclosePoints"
  sdlEnclosePoints :: Ptr Point -> #{type int} -> Ptr Rect -> Ptr Rect -> IO #{type SDL_bool}

enclosePoints :: [Point] -> Maybe Rect -> IO Rect
enclosePoints points clip =
  alloca $ \rect' ->
  allocaArray (length points) $ \points' -> do
    pokeArray points' points
    let count = fromIntegral (length points)
    r <- maybeWith with clip $ \clip' -> sdlEnclosePoints points' count clip' rect'
    rect <- peek rect'
    handleErrorI "enclosePoints" r $ const $ return rect

foreign import ccall unsafe "SDL_RectEmpty_Wrapper"
  sdlRectEmpty :: Ptr Rect -> IO #{type SDL_bool}

rectEmpty :: Rect -> IO Bool
rectEmpty rect =
  (with rect $ sdlRectEmpty) >>= return . sdlBoolToBool

foreign import ccall unsafe "SDL_RectEquals_Wrapper"
  sdlRectEquals :: Ptr Rect -> Ptr Rect -> IO #{type SDL_bool}

rectEquals :: Rect -> Rect -> IO Bool
rectEquals a b =
  with a $ \a' ->
  with b $ \b' ->
    sdlRectEquals a' b' >>= return . sdlBoolToBool

foreign import ccall unsafe "SDL_HasIntersection"
  sdlHasIntersection :: Ptr Rect -> Ptr Rect -> IO #{type SDL_bool}

hasIntersection :: Rect -> Rect -> IO Bool
hasIntersection a b =
  with a $ \a' ->
  with b $ \b' ->
    sdlHasIntersection a' b' >>= return . sdlBoolToBool

foreign import ccall unsafe "SDL_IntersectRect"
  sdlIntersectRect :: Ptr Rect -> Ptr Rect -> Ptr Rect -> IO #{type SDL_bool}

intersectRect :: Rect -> Rect -> IO Rect
intersectRect a b =
  with a $ \a' ->
  with b $ \b' -> do
    alloca $ \c' -> do
      r <- sdlIntersectRect a' b' c'
      c <- peek c'
      handleErrorI "intersectRect" r $ const $ return c

foreign import ccall unsafe "SDL_IntersectRectAndLine"
  sdlIntersectRectAndLine :: Ptr Rect -> Ptr #{type int} -> Ptr #{type int}
                             -> Ptr #{type int} -> Ptr #{type int} -> IO #{type SDL_bool}

intersectRectAndLine :: Rect -> Int -> Int -> Int -> Int -> IO Bool
intersectRectAndLine r x1 y1 x2 y2 =
  with r $ \r' ->
  with (fromIntegral x1) $ \x1' ->
  with (fromIntegral y1) $ \y1' ->
  with (fromIntegral x2) $ \x2' ->
  with (fromIntegral y2) $ \y2' ->
    sdlIntersectRectAndLine r' x1' y1' x2' y2' >>= return . sdlBoolToBool

foreign import ccall unsafe "SDL_UnionRect"
  sdlUnionRect :: Ptr Rect -> Ptr Rect -> Ptr Rect -> IO ()

unionRect :: Rect -> Rect -> IO Rect
unionRect a b =
  with a $ \a' ->
  with b $ \b' ->
    alloca $ \c' -> do
      sdlUnionRect a' b' c'
      handleError "unionRect" c' peek

