-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.SDL.TTF.Render
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# CFILES Graphics/UI/SDL/TTF/Wrapper.c #-}

module Graphics.UI.SDL.TTF.Render
    ( tryRenderTextSolid
    , renderTextSolid
    , tryRenderUTF8Solid
    , renderUTF8Solid
    , tryRenderGlyphSolid
    , renderGlyphSolid

    , tryRenderTextShaded
    , renderTextShaded
    , tryRenderUTF8Shaded
    , renderUTF8Shaded
    , tryRenderGlyphShaded
    , renderGlyphShaded

    , tryRenderTextBlended
    , renderTextBlended
    , tryRenderUTF8Blended
    , renderUTF8Blended
    , tryRenderGlyphBlended
    , renderGlyphBlended

    ) where

import Graphics.UI.SDL.TTF.Types
import Graphics.UI.SDL.General
import Graphics.UI.SDL.Video
import Graphics.UI.SDL.Color
import Graphics.UI.SDL.Types
import Foreign
import Foreign.C

import Data.Char

renderOneColor :: (Ptr FontStruct -> CString -> Ptr Color -> IO (Ptr SurfaceStruct))
            -> Font -> String -> Color -> IO (Maybe Surface)
renderOneColor render font text color
    = withForeignPtr font $ \fontPtr ->
      withCString text $ \cString ->
      with color $ \colorPtr ->
      do image <- render fontPtr cString colorPtr
         if image == nullPtr
            then return Nothing
            else fmap Just (mkFinalizedSurface image)

renderTwoColor :: (Ptr FontStruct -> CString -> Ptr Color -> Ptr Color -> IO (Ptr SurfaceStruct))
            -> Font -> String -> Color -> Color -> IO (Maybe Surface)
renderTwoColor render font text color1 color2
    = withForeignPtr font $ \fontPtr ->
      withCString text $ \cString ->
      with color1 $ \colorPtr1 ->
      with color2 $ \colorPtr2 -> 
      do image <- render fontPtr cString colorPtr1 colorPtr2
         if image == nullPtr
            then return Nothing
            else fmap Just (mkFinalizedSurface image)

--------------------------------------------------------------
-- Solid
--------------------------------------------------------------


-- SDL_Surface *TTF_RenderText_Solid(TTF_Font *font, const char *text, SDL_Color fg)
foreign import ccall unsafe "renderTextSolid" ttfRenderTextSolid
    :: Ptr FontStruct -> CString -> Ptr Color -> IO (Ptr SurfaceStruct)
tryRenderTextSolid :: Font -> String -> Color -> IO (Maybe Surface)
tryRenderTextSolid = renderOneColor ttfRenderTextSolid

renderTextSolid :: Font -> String -> Color -> IO Surface
renderTextSolid font text color
    = unwrapMaybe "TTF_RenderText_Solid" (tryRenderTextSolid font text color)

-- SDL_Surface * SDLCALL TTF_RenderUTF8_Solid(TTF_Font *font, const char *text, SDL_Color fg);
foreign import ccall unsafe "renderUTF8Solid" ttfRenderUTF8Solid
    :: Ptr FontStruct -> CString -> Ptr Color -> IO (Ptr SurfaceStruct)
tryRenderUTF8Solid :: Font -> String -> Color -> IO (Maybe Surface)
tryRenderUTF8Solid = renderOneColor ttfRenderUTF8Solid

renderUTF8Solid :: Font -> String -> Color -> IO Surface
renderUTF8Solid font text color
    = unwrapMaybe "TTF_RenderUTF8_Solid" (tryRenderUTF8Solid font text color)

-- SDL_Surface * renderGlyphSolid(TTF_Font *font, Uint16 ch, SDL_Color *fg);
foreign import ccall unsafe "renderGlyphSolid" renderGlyphSolid
    :: Ptr FontStruct -> Word16 -> Ptr Color -> IO (Ptr SurfaceStruct)
tryRenderGlyphSolid :: Font -> Char -> Color -> IO (Maybe Surface)
tryRenderGlyphSolid font ch fg
    = withForeignPtr font $ \fontPtr ->
      with fg $ \color ->
      do image <- renderGlyphSolid fontPtr (fromIntegral (ord ch)) color
         if image == nullPtr
            then return Nothing
            else fmap Just (mkFinalizedSurface image)

--------------------------------------------------------------
-- Shaded
--------------------------------------------------------------


-- SDL_Surface *TTF_RenderText_Shaded(TTF_Font *font, const char *text, SDL_Color fg)
foreign import ccall unsafe "renderTextShaded" ttfRenderTextShaded
    :: Ptr FontStruct -> CString -> Ptr Color -> Ptr Color -> IO (Ptr SurfaceStruct)
tryRenderTextShaded :: Font -> String -> Color -> Color -> IO (Maybe Surface)
tryRenderTextShaded = renderTwoColor ttfRenderTextShaded

renderTextShaded :: Font -> String -> Color -> Color -> IO Surface
renderTextShaded font text fg bg
    = unwrapMaybe "TTF_RenderText_Shaded" (tryRenderTextShaded font text fg bg)

-- SDL_Surface * SDLCALL TTF_RenderUTF8_Shaded(TTF_Font *font, const char *text, SDL_Color fg);
foreign import ccall unsafe "renderUTF8Shaded" ttfRenderUTF8Shaded
    :: Ptr FontStruct -> CString -> Ptr Color -> Ptr Color -> IO (Ptr SurfaceStruct)
tryRenderUTF8Shaded :: Font -> String -> Color -> Color -> IO (Maybe Surface)
tryRenderUTF8Shaded = renderTwoColor ttfRenderUTF8Shaded

renderUTF8Shaded :: Font -> String -> Color -> Color -> IO Surface
renderUTF8Shaded font text fg bg
    = unwrapMaybe "TTF_RenderUTF8_Shaded" (tryRenderUTF8Shaded font text fg bg)

-- SDL_Surface * renderGlyphShaded(TTF_Font *font, Uint16 ch, SDL_Color *fg);
foreign import ccall unsafe "renderGlyphShaded" renderGlyphShaded
    :: Ptr FontStruct -> Word16 -> Ptr Color -> Ptr Color -> IO (Ptr SurfaceStruct)
tryRenderGlyphShaded :: Font -> Char -> Color -> Color -> IO (Maybe Surface)
tryRenderGlyphShaded font ch fg bg
    = withForeignPtr font $ \fontPtr ->
      with fg $ \fgPtr ->
      with bg $ \bgPtr ->
      do image <- renderGlyphShaded fontPtr (fromIntegral (ord ch)) fgPtr bgPtr
         if image == nullPtr
            then return Nothing
            else fmap Just (mkFinalizedSurface image)

--------------------------------------------------------------
-- Blended
--------------------------------------------------------------


-- SDL_Surface *TTF_RenderText_Blended(TTF_Font *font, const char *text, SDL_Color fg)
foreign import ccall unsafe "renderTextBlended" ttfRenderTextBlended
    :: Ptr FontStruct -> CString -> Ptr Color -> IO (Ptr SurfaceStruct)
tryRenderTextBlended :: Font -> String -> Color -> IO (Maybe Surface)
tryRenderTextBlended = renderOneColor ttfRenderTextBlended

renderTextBlended :: Font -> String -> Color -> IO Surface
renderTextBlended font text color
    = unwrapMaybe "TTF_RenderText_Blended" (tryRenderTextBlended font text color)

-- SDL_Surface * SDLCALL TTF_RenderUTF8_Blended(TTF_Font *font, const char *text, SDL_Color fg);
foreign import ccall unsafe "renderUTF8Blended" ttfRenderUTF8Blended
    :: Ptr FontStruct -> CString -> Ptr Color -> IO (Ptr SurfaceStruct)
tryRenderUTF8Blended :: Font -> String -> Color -> IO (Maybe Surface)
tryRenderUTF8Blended = renderOneColor ttfRenderUTF8Blended

renderUTF8Blended :: Font -> String -> Color -> IO Surface
renderUTF8Blended font text color
    = unwrapMaybe "TTF_RenderUTF8_Blended" (tryRenderUTF8Blended font text color)

-- SDL_Surface * renderGlyphBlended(TTF_Font *font, Uint16 ch, SDL_Color *fg);
foreign import ccall unsafe "renderGlyphBlended" renderGlyphBlended
    :: Ptr FontStruct -> Word16 -> Ptr Color -> IO (Ptr SurfaceStruct)
tryRenderGlyphBlended :: Font -> Char -> Color -> IO (Maybe Surface)
tryRenderGlyphBlended font ch fg
    = withForeignPtr font $ \fontPtr ->
      with fg $ \color ->
      do image <- renderGlyphBlended fontPtr (fromIntegral (ord ch)) color
         if image == nullPtr
            then return Nothing
            else fmap Just (mkFinalizedSurface image)



