-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.SDL.TTF.Management
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Graphics.UI.SDL.TTF.Management
    ( tryOpenFont
    , openFont
    , closeFont
    , tryOpenFontRW
    , openFontRW
    ) where

import Graphics.UI.SDL.TTF.Types
import Graphics.UI.SDL.General (unwrapMaybe,unwrapBool)
import Graphics.UI.SDL.RWOps
import Graphics.UI.SDL.Types

import Foreign
import Foreign.C

-- void TTF_CloseFont(TTF_Font *font)
foreign import ccall unsafe "&TTF_CloseFont" ttfCloseFontFinal :: FunPtr (Ptr FontStruct -> IO ())
mkFinalizedFont :: Ptr FontStruct -> IO Font
mkFinalizedFont = newForeignPtr ttfCloseFontFinal

foreign import ccall unsafe "TTF_CloseFont" ttfCloseFont :: Ptr FontStruct -> IO ()
closeFont :: Font -> IO ()
closeFont font = withForeignPtr font ttfCloseFont

-- TTF_Font *TTF_OpenFont(const char *file, int ptsize)
foreign import ccall unsafe "TTF_OpenFont" ttfOpenFont :: CString -> Int -> IO (Ptr FontStruct)
tryOpenFont :: String -> Int -> IO (Maybe Font)
tryOpenFont path ptsize
    = withCString path $ \cPath ->
      do font <- ttfOpenFont cPath ptsize
         if font == nullPtr
            then return Nothing
            else fmap Just (mkFinalizedFont font)

openFont :: String -> Int -> IO Font
openFont path ptsize = unwrapMaybe "TTF_OpenFont" (tryOpenFont path ptsize)

-- TTF_Font *TTF_OpenFontRW(SDL_RWops *src, int freesrc, int ptsize)
foreign import ccall unsafe "TTF_OpenFontRW" ttfOpenFontRW :: Ptr RWopsStruct -> Int -> Int -> IO (Ptr FontStruct)
tryOpenFontRW :: RWops -> Bool -> Int -> IO (Maybe Font)
tryOpenFontRW rw freesrc ptsize
    = withForeignPtr rw $ \rwPtr ->
      do font <- ttfOpenFontRW rwPtr (fromBool freesrc) ptsize
         if font == nullPtr
            then return Nothing
            else fmap Just (mkFinalizedFont font)

openFontRW :: RWops -> Bool -> Int -> IO Font
openFontRW rw freesrc ptsize
    = unwrapMaybe "TTF_OpenFontRW" (tryOpenFontRW rw freesrc ptsize)


-- TODO:
-- TTF_Font *TTF_OpenFontIndex(const char *file, int ptsize, long index)
-- TTF_Font *TTF_OpenFontIndexRW(SDL_RWops *src, int freesrc, int ptsize, long index)

