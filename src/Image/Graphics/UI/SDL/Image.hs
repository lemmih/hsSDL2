-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.SDL.Image
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Graphics.UI.SDL.Image
    ( load
    , loadRW
    , ImageType(..)
    , loadTypedRW
    , loadTyped
    , isTypedRW
    , isTyped
    , isBMPRW
    , isBMP
    , isPNMRW
    , isPNM
    , isXPMRW
    , isXPM
    , isXCFRW
    , isXCF
    , isPCXRW
    , isPCX
    , isGIFRW
    , isGIF
    , isJPGRW
    , isJPG
    , isTIFRW
    , isTIF
    , isPNGRW
    , isPNG
    , isLBMRW
    , isLBM
    , module Graphics.UI.SDL.Image.Version
    ) where

import Graphics.UI.SDL.Image.Version

import Foreign
import Foreign.C

import Graphics.UI.SDL.Video
import Graphics.UI.SDL.General
import Graphics.UI.SDL.Types
import Graphics.UI.SDL.RWOps as RW

finalizeWhenNotNull :: String -> Ptr SurfaceStruct -> IO Surface
finalizeWhenNotNull errMsg image
    = if image == nullPtr
         then failWithError errMsg
         else mkFinalizedSurface image

-- SDL_Surface *IMG_Load(const char *file)
foreign import ccall unsafe "IMG_Load" imgLoad :: CString -> IO (Ptr SurfaceStruct)
load :: FilePath -> IO Surface
load filepath
    = withCString filepath $ \cPath ->
      imgLoad cPath >>= finalizeWhenNotNull "IMG_Load"

-- SDL_Surface *IMG_Load_RW(SDL_RWops *src, int freesrc)
foreign import ccall "IMG_Load_RW" imgLoadRW :: Ptr RWopsStruct -> Int -> IO (Ptr SurfaceStruct)
loadRW :: RWops -> Bool -> IO Surface
loadRW rw freesrc
    = withForeignPtr rw $ \rwPtr ->
      imgLoadRW rwPtr (fromBool freesrc) >>= finalizeWhenNotNull "IMG_Load_RW"



data ImageType
    = TGA
    | BMP
    | PNM
    | XPM
    | XCF
    | PCX
    | GIF
    | JPG
    | TIF
    | LBM
    | PNG
    | Other String

imageTypeToString :: ImageType -> String
imageTypeToString TGA = "TGA"
imageTypeToString BMP = "BMP"
imageTypeToString PNM = "PNM"
imageTypeToString XPM = "XPM"
imageTypeToString XCF = "XCF"
imageTypeToString PCX = "PCX"
imageTypeToString GIF = "GIF"
imageTypeToString JPG = "JPG"
imageTypeToString TIF = "TIF"
imageTypeToString LBM = "LBM"
imageTypeToString PNG = "PNG"
imageTypeToString (Other format) = format

-- SDL_Surface *IMG_LoadTyped_RW(SDL_RWops *src, int freesrc, char *type)
foreign import ccall "IMG_LoadTyped_RW" imgLoadTypedRW :: Ptr RWopsStruct -> Int -> CString -> IO (Ptr SurfaceStruct)
loadTypedRW :: RWops -> Bool -> ImageType -> IO Surface
loadTypedRW rw freesrc imageType
    = withForeignPtr rw $ \rwPtr ->
      withCString (imageTypeToString imageType) $ \cType ->
      imgLoadTypedRW rwPtr (fromBool freesrc) cType >>= finalizeWhenNotNull "IMG_LoadTyped_RW"

loadTyped :: FilePath -> ImageType -> IO Surface
loadTyped filepath imageType
    = RW.with filepath "rb" $ \rw ->
      loadTypedRW rw False imageType

isTypedRW :: ImageType -> RWops -> IO Bool
isTypedRW BMP = isBMPRW
isTypedRW PNM = isPNMRW
isTypedRW XPM = isXPMRW
isTypedRW XCF = isXCFRW
isTypedRW PCX = isPCXRW
isTypedRW GIF = isGIFRW
isTypedRW JPG = isJPGRW
isTypedRW TIF = isTIFRW
isTypedRW PNG = isPNGRW
isTypedRW LBM = isLBMRW
isTypedRW _other = const (return False)

isTyped :: ImageType -> FilePath -> IO Bool
isTyped imageType path
    = isTypedRW imageType =<< RW.fromFile path "rb"


-- int IMG_isBMP(SDL_RWops *src)
foreign import ccall unsafe "IMG_isBMP" imgIsBMP :: Ptr RWopsStruct -> IO Int
isBMPRW :: RWops -> IO Bool
isBMPRW rw
    = withForeignPtr rw $ \rwPtr ->
      fmap toBool (imgIsBMP rwPtr)

isBMP :: FilePath -> IO Bool
isBMP path = RW.with path "rb" $
             isBMPRW

-- int IMG_isPNM(SDL_RWops *src)
foreign import ccall unsafe "IMG_isPNM" imgIsPNM :: Ptr RWopsStruct -> IO Int
isPNMRW :: RWops -> IO Bool
isPNMRW rw
    = withForeignPtr rw $ \rwPtr ->
      fmap toBool (imgIsPNM rwPtr)

isPNM :: FilePath -> IO Bool
isPNM path = RW.with path "rb" $
             isPNMRW

-- int IMG_isXPM(SDL_RWops *src)
foreign import ccall unsafe "IMG_isXPM" imgIsXPM :: Ptr RWopsStruct -> IO Int
isXPMRW :: RWops -> IO Bool
isXPMRW rw
    = withForeignPtr rw $ \rwPtr ->
      fmap toBool (imgIsXPM rwPtr)

isXPM :: FilePath -> IO Bool
isXPM path = RW.with path "rb" $
             isXPMRW

-- int IMG_isXCF(SDL_RWops *src)
foreign import ccall unsafe "IMG_isXCF" imgIsXCF :: Ptr RWopsStruct -> IO Int
isXCFRW :: RWops -> IO Bool
isXCFRW rw
    = withForeignPtr rw $ \rwPtr ->
      fmap toBool (imgIsXCF rwPtr)

isXCF :: FilePath -> IO Bool
isXCF path = RW.with path "rb" $
             isXCFRW


-- int IMG_isPCX(SDL_RWops *src)
foreign import ccall unsafe "IMG_isPCX" imgIsPCX :: Ptr RWopsStruct -> IO Int
isPCXRW :: RWops -> IO Bool
isPCXRW rw
    = withForeignPtr rw $ \rwPtr ->
      fmap toBool (imgIsPCX rwPtr)

isPCX :: FilePath -> IO Bool
isPCX path = RW.with path "rb" $
             isPCXRW


-- int IMG_isGIF(SDL_RWops *src)
foreign import ccall unsafe "IMG_isGIF" imgIsGIF :: Ptr RWopsStruct -> IO Int
isGIFRW :: RWops -> IO Bool
isGIFRW rw
    = withForeignPtr rw $ \rwPtr ->
      fmap toBool (imgIsGIF rwPtr)

isGIF :: FilePath -> IO Bool
isGIF path = RW.with path "rb" $
             isGIFRW


-- int IMG_isJPG(SDL_RWops *src)
foreign import ccall unsafe "IMG_isJPG" imgIsJPG :: Ptr RWopsStruct -> IO Int
isJPGRW :: RWops -> IO Bool
isJPGRW rw
    = withForeignPtr rw $ \rwPtr ->
      fmap toBool (imgIsJPG rwPtr)

isJPG :: FilePath -> IO Bool
isJPG path = RW.with path "rb" $
             isJPGRW


-- int IMG_isTIF(SDL_RWops *src)
foreign import ccall unsafe "IMG_isTIF" imgIsTIF :: Ptr RWopsStruct -> IO Int
isTIFRW :: RWops -> IO Bool
isTIFRW rw
    = withForeignPtr rw $ \rwPtr ->
      fmap toBool (imgIsTIF rwPtr)

isTIF :: FilePath -> IO Bool
isTIF path = RW.with path "rb" $
             isTIFRW


-- int IMG_isPNG(SDL_RWops *src)
foreign import ccall unsafe "IMG_isPNG" imgIsPNG :: Ptr RWopsStruct -> IO Int
isPNGRW :: RWops -> IO Bool
isPNGRW rw
    = withForeignPtr rw $ \rwPtr ->
      fmap toBool (imgIsPNG rwPtr)

isPNG :: FilePath -> IO Bool
isPNG path = RW.with path "rb" $
             isPNGRW


-- int IMG_isLBM(SDL_RWops *src)
foreign import ccall unsafe "IMG_isLBM" imgIsLBM :: Ptr RWopsStruct -> IO Int
isLBMRW :: RWops -> IO Bool
isLBMRW rw
    = withForeignPtr rw $ \rwPtr ->
      fmap toBool (imgIsLBM rwPtr)

isLBM :: FilePath -> IO Bool
isLBM path = RW.with path "rb" $
             isLBMRW


