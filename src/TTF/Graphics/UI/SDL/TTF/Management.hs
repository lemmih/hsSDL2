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
    , tryOpenFontIndex
    , openFontIndex
    ) where

import Graphics.UI.SDL.TTF.Types
import Graphics.UI.SDL.General (unwrapMaybe)
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
      ttfOpenFont cPath ptsize >>= maybePeek mkFinalizedFont

openFont :: String -> Int -> IO Font
openFont path ptsize = unwrapMaybe "TTF_OpenFont" (tryOpenFont path ptsize)

-- TTF_Font *TTF_OpenFontRW(SDL_RWops *src, int freesrc, int ptsize)
foreign import ccall unsafe "TTF_OpenFontRW" ttfOpenFontRW :: Ptr RWopsStruct -> Int -> Int -> IO (Ptr FontStruct)
tryOpenFontRW :: RWops -> Bool -> Int -> IO (Maybe Font)
tryOpenFontRW rw freesrc ptsize
    = withForeignPtr rw $ \rwPtr ->
      ttfOpenFontRW rwPtr (fromBool freesrc) ptsize >>= maybePeek mkFinalizedFont

openFontRW :: RWops -> Bool -> Int -> IO Font
openFontRW rw freesrc ptsize
    = unwrapMaybe "TTF_OpenFontRW" (tryOpenFontRW rw freesrc ptsize)

-- TTF_Font *TTF_OpenFontIndex(const char *file, int ptsize, long index)
foreign import ccall unsafe "TTF_OpenFontIndex" ttfOpenFontIndex :: CString -> Int -> Int -> IO (Ptr FontStruct)
tryOpenFontIndex :: String -> Int -> Int -> IO (Maybe Font)
tryOpenFontIndex file ptsize index
    = withCString file $ \cFile ->
      ttfOpenFontIndex cFile ptsize index >>= maybePeek mkFinalizedFont

openFontIndex :: String -> Int -> Int -> IO Font
openFontIndex file ptsize index = unwrapMaybe "TTF_OpenFontIndex" (tryOpenFontIndex file ptsize index)

-- TODO:
-- 
-- TTF_Font *TTF_OpenFontIndexRW(SDL_RWops *src, int freesrc, int ptsize, long index)

