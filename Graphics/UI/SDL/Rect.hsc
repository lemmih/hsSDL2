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
    , rectEquals
    , rectEmpty
    , hasIntersection
    , intersectRect
    , unionRect
    ) where

import Foreign
import Foreign.C
import Graphics.UI.SDL.General
import Graphics.UI.SDL.Utilities

data Rect
    = Rect
    { rectX, rectY :: Int,
      rectW, rectH :: Int }
    deriving (Show,Eq,Ord)

instance Storable Rect where
    sizeOf = const #{size SDL_Rect}
    alignment = const 2
    peek ptr
        = do x <- #{peek SDL_Rect, x} ptr :: IO CInt
             y <- #{peek SDL_Rect, y} ptr :: IO CInt
             w <- #{peek SDL_Rect, w} ptr :: IO CInt
             h <- #{peek SDL_Rect, h} ptr :: IO CInt
             return $! Rect (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)
    poke ptr (Rect x y w h)
        = do #{poke SDL_Rect, x} ptr (fromIntegral x :: CInt)
             #{poke SDL_Rect, y} ptr (fromIntegral y :: CInt)
             #{poke SDL_Rect, w} ptr (fromIntegral w :: CInt)
             #{poke SDL_Rect, h} ptr (fromIntegral h :: CInt)

data Point = Point { pX :: Int, pY :: Int }

instance Storable Point where
  sizeOf = const #{size SDL_Point}

  alignment = const 2

  peek ptr = do
    x <- #{peek SDL_Point, x} ptr :: IO CInt
    y <- #{peek SDL_Point, y} ptr :: IO CInt
    return $! Point (fromIntegral x) (fromIntegral y)

  poke ptr (Point x y) = do
    #{poke SDL_Point, x} ptr (fromIntegral x :: CInt)
    #{poke SDL_Point, y} ptr (fromIntegral y :: CInt)

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

foreign import ccall unsafe "SDL_UnionRect"
  sdlUnionRect :: Ptr Rect -> Ptr Rect -> Ptr Rect -> IO ()

unionRect :: Rect -> Rect -> IO Rect
unionRect a b =
  with a $ \a' ->
  with b $ \b' ->
    alloca $ \c' -> do
      sdlUnionRect a' b' c'
      handleError "unionRect" c' peek

