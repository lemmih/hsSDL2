-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.SDL.TTF.Attributes
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Graphics.UI.SDL.TTF.Attributes
    ( getFontStyle
    , setFontStyle
    , fontHeight
    , fontAscent
    , fontDescent
    , fontLineSkip
    , fontFaces
    , fontFaceIsFixedWidth
    , fontFaceFamilyName
    , fontFaceStyleName
    , tryTextSize
    , textSize
    , tryUTF8Size
    , utf8Size
    , FontStyle(..)
    ) where

import Foreign
import Foreign.C

import Graphics.UI.SDL.TTF.Types
import Graphics.UI.SDL.Utilities
import Graphics.UI.SDL.General

data FontStyle
    = StyleBold
    | StyleItalic
    | StyleUnderline
      deriving (Show,Eq,Ord)

instance Bounded FontStyle where
    minBound = StyleBold
    maxBound = StyleUnderline

instance Enum FontStyle where
    fromEnum StyleBold = 1
    fromEnum StyleItalic = 2
    fromEnum StyleUnderline = 4
    toEnum 1 = StyleBold
    toEnum 2 = StyleItalic
    toEnum 4 = StyleUnderline
    toEnum _ = error "Graphics.UI.SDL.TTF.Attributes.toEnum: bad argument"
    succ StyleBold = StyleItalic
    succ StyleItalic = StyleUnderline
    succ _ = error "Graphics.UI.SDL.TTF.Attributes.succ: bad argument"
    pred StyleItalic = StyleBold
    pred StyleUnderline = StyleItalic
    pred _ = error "Graphics.UI.SDL.TTF.Attributes.pred: bad argument"
    enumFromTo x y | x > y = []
                   | x == y = [y]
                   | True = x : enumFromTo (succ x) y


-- int TTF_GetFontStyle(TTF_Font *font)
foreign import ccall unsafe "TTF_GetFontStyle" ttfGetFontStyle :: Ptr FontStruct -> IO Int
getFontStyle :: Font -> IO [FontStyle]
getFontStyle font
    = withForeignPtr font $
      fmap fromBitmask . ttfGetFontStyle 

-- void TTF_SetFontStyle(TTF_Font *font, int style)
foreign import ccall unsafe "TTF_SetFontStyle" ttfSetFontStyle :: Ptr FontStruct -> Int -> IO ()
setFontStyle :: Font -> [FontStyle] -> IO ()
setFontStyle font style
    = withForeignPtr font $ \fontPtr ->
      ttfSetFontStyle fontPtr (toBitmask style)

-- int TTF_FontHeight(TTF_Font *font)
foreign import ccall unsafe "TTF_FontHeight" ttfFontHeight :: Ptr FontStruct -> IO Int
fontHeight :: Font -> IO Int
fontHeight font = withForeignPtr font ttfFontHeight

-- int TTF_FontAscent(TTF_Font *font)
foreign import ccall unsafe "TTF_FontAscent" ttfFontAscent :: Ptr FontStruct -> IO Int
fontAscent :: Font -> IO Int
fontAscent font = withForeignPtr font ttfFontAscent

-- int TTF_FontDecent(TTF_Font *font)
foreign import ccall unsafe "TTF_FontAscent" ttfFontDescent :: Ptr FontStruct -> IO Int
fontDescent :: Font -> IO Int
fontDescent font = withForeignPtr font ttfFontDescent

-- int TTF_FontLineSkip(TTF_Font *font)
foreign import ccall unsafe "TTF_FontLineSkip" ttfFontLineSkip :: Ptr FontStruct -> IO Int
fontLineSkip :: Font -> IO Int
fontLineSkip font = withForeignPtr font ttfFontLineSkip

-- long TTF_FontFaces(TTF_Font *font);
foreign import ccall unsafe "TTF_FontFaces" ttfFontFaces :: Ptr FontStruct -> IO Int
fontFaces :: Font -> IO Int
fontFaces font = withForeignPtr font ttfFontFaces

-- int SDLCALL TTF_FontFaceIsFixedWidth(TTF_Font *font);
foreign import ccall unsafe "TTF_FontFaceIsFixedWidth" ttfFontFaceIsFixedWidth :: Ptr FontStruct -> IO Int
fontFaceIsFixedWidth :: Font -> IO Int
fontFaceIsFixedWidth font = withForeignPtr font ttfFontFaceIsFixedWidth

-- char * SDLCALL TTF_FontFaceFamilyName(TTF_Font *font);
foreign import ccall unsafe "TTF_FontFaceFamilyName" ttfFontFaceFamilyName :: Ptr FontStruct -> IO CString
fontFaceFamilyName :: Font -> IO String
fontFaceFamilyName font = withForeignPtr font ttfFontFaceFamilyName >>= peekCString

-- char * SDLCALL TTF_FontFaceStyleName(TTF_Font *font);
foreign import ccall unsafe "TTF_FontFaceStyleName" ttfFontFaceStyleName :: Ptr FontStruct -> IO CString
fontFaceStyleName :: Font -> IO String
fontFaceStyleName font = withForeignPtr font ttfFontFaceStyleName >>= peekCString


getSize :: (Ptr FontStruct -> CString -> Ptr Int -> Ptr Int -> IO Int) -> Font -> String -> IO (Maybe (Int,Int))
getSize getter font string
    = withCString string $ \cString ->
      alloca $ \width ->
      alloca $ \height ->
      withForeignPtr font $ \fontPtr ->
      do ret <- getter fontPtr cString width height
         case ret of
           0 -> do [w,h] <- mapM peek [width,height]
                   return (Just (w,h))
           _ -> return Nothing

-- int SDLCALL TTF_SizeText(TTF_Font *font, const char *text, int *w, int *h);
foreign import ccall unsafe "TTF_SizeText" ttfSizeText
    :: Ptr FontStruct -> CString -> Ptr Int -> Ptr Int -> IO Int
tryTextSize :: Font -> String -> IO (Maybe (Int,Int))
tryTextSize = getSize ttfSizeText

textSize :: Font -> String -> IO (Int,Int)
textSize font string = unwrapMaybe "TTF_SizeText" (tryTextSize font string)

-- int SDLCALL TTF_SizeUTF8(TTF_Font *font, const char *text, int *w, int *h);
foreign import ccall unsafe "TTF_SizeUTF8" ttfSizeUTF8
    :: Ptr FontStruct -> CString -> Ptr Int -> Ptr Int -> IO Int
tryUTF8Size :: Font -> String -> IO (Maybe (Int,Int))
tryUTF8Size = getSize ttfSizeUTF8

utf8Size :: Font -> String -> IO (Int,Int)
utf8Size font string = unwrapMaybe "TTF_SizeUTF8" (tryUTF8Size font string)

