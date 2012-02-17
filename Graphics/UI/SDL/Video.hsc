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
module Graphics.UI.SDL.Video
    ( Palette
    , Toggle (..)
    , fromToggle
    , toToggle
    , tryGetVideoSurface
    , getVideoSurface
    , tryVideoDriverName
    , videoDriverName
    , getVideoInfo
    , ListModes(..)
    , listModes
    , videoModeOK
    , trySetVideoMode
    , setVideoMode
    , updateRect
    , updateRects
    , tryFlip
    , flip
    , setColors
    , setPalette
    , setGamma
    , tryGetGammaRamp
    , getGammaRamp
    , trySetGammaRamp
    , setGammaRamp
    , mapRGB
    , mapRGBA
    , getRGB
    , getRGBA
    , tryCreateRGBSurface
    , createRGBSurface
    , tryCreateRGBSurfaceEndian
    , createRGBSurfaceEndian
    , tryCreateRGBSurfaceFrom
    , createRGBSurfaceFrom
    , freeSurface
    , lockSurface
    , unlockSurface
    , loadBMP
    , saveBMP
    , setColorKey
    , setAlpha
    , setClipRect
    , getClipRect
    , withClipRect
    , tryConvertSurface
    , convertSurface
    , blitSurface
    , fillRect
    , tryDisplayFormat
    , displayFormat
    , tryDisplayFormatAlpha
    , displayFormatAlpha
    , warpMouse
    , showCursor
    , queryCursorState
    , GLAttr, GLValue
    , glRedSize, glGreenSize, glBlueSize, glAlphaSize, glBufferSize, glDoubleBuffer
    , glDepthSize, glStencilSize, glAccumRedSize, glAccumGreenSize, glAccumBlueSize
    , glAccumAlphaSize, glStereo, glMultiSampleBuffers, glMultiSampleSamples
    , tryGLGetAttribute, glGetAttribute
    , tryGLSetAttribute, glSetAttribute
    , glSwapBuffers
    , mkFinalizedSurface
    ) where

#include <SDL.h>
#ifdef main
#undef main
#endif

import Foreign (Ptr, FunPtr, Storable(peek), castPtr, plusPtr, nullPtr, newForeignPtr_,
               finalizeForeignPtr, alloca, withForeignPtr, newForeignPtr)
import Foreign.C (peekCString, CString, CInt(..))
import Foreign.Marshal.Array (withArrayLen, peekArray0, peekArray, allocaArray)
import Foreign.Marshal.Utils (with, toBool, maybeWith, maybePeek, fromBool)
import Control.Exception (bracket)
import Data.Word (Word8, Word16, Word32)
import Data.Int (Int32)

import Graphics.UI.SDL.Utilities (Enum(..), intToBool, toBitmask, fromCInt, toCInt)
import Graphics.UI.SDL.General (unwrapMaybe, unwrapBool)
import Graphics.UI.SDL.Rect (Rect(rectY, rectX, rectW, rectH))
import Graphics.UI.SDL.Color (Pixel(..), Color)
import Graphics.UI.SDL.Types (SurfaceFlag, PixelFormat, PixelFormatStruct, RWops,
                              RWopsStruct, VideoInfo, VideoInfoStruct, Surface, SurfaceStruct)
import qualified Graphics.UI.SDL.RWOps as RW

import Prelude hiding (flip,Enum(..))

--import Foreign.HacanonLight.Generate
--import Foreign.HacanonLight.DIS (foreignPtr,mkOut,word8)

data Palette
    = Logical
    | Physical
      deriving (Show,Eq,Ord)

instance Bounded Palette where
    minBound = Logical
    maxBound = Physical

instance Enum Palette Int where
    fromEnum Logical = #{const SDL_LOGPAL}
    fromEnum Physical = #{const SDL_PHYSPAL}
    toEnum #{const SDL_LOGPAL} = Logical
    toEnum #{const SDL_PHYSPAL} = Physical
    toEnum _ = error "Graphics.UI.SDL.Video.toEnum: bad argument"
    succ Logical = Physical
    succ _ = error "Graphics.UI.SDL.Video.succ: bad argument"
    pred Physical = Logical
    pred _ = error "Graphics.UI.SDL.Video.pred: bad argument"
    enumFromTo x y | x > y = []
                   | x == y = [y]
                   | True = x : enumFromTo (succ x) y


data Toggle = Disable | Enable | Query
    deriving (Eq, Ord, Show)

toToggle :: (Eq a, Num a) => a -> Toggle
toToggle (#{const SDL_DISABLE}) = Disable
toToggle (#{const SDL_ENABLE}) = Enable
toToggle (#{const SDL_QUERY}) = Query
toToggle _ = error "Graphics.UI.SDL.Video.toToggle: bad argument"

fromToggle :: (Num a) => Toggle -> a
fromToggle Disable = 0
fromToggle Enable = 1
fromToggle Query = (-1)

foreign import ccall unsafe "SDL_GetVideoSurface" sdlGetVideoSurface :: IO (Ptr SurfaceStruct)

-- | Returns the video surface or @Nothing@ on error.
tryGetVideoSurface :: IO (Maybe Surface)
tryGetVideoSurface =
    sdlGetVideoSurface >>= maybePeek newForeignPtr_

-- | Returns the video surface, throwing an exception on error.
getVideoSurface :: IO Surface
getVideoSurface = unwrapMaybe "SDL_GetVideoSurface" tryGetVideoSurface

foreign import ccall unsafe "SDL_VideoDriverName" sdlVideoDriverName :: CString -> CInt -> IO CString

-- | Returns the video driver name or @Nothing@ on error. Notice, the driver name is limited to 256 chars.
tryVideoDriverName :: IO (Maybe String)
tryVideoDriverName 
    = allocaArray size (\ptr -> sdlVideoDriverName ptr (toCInt size) >>= maybePeek peekCString)
    where size = 256

-- | Returns the video driver name, throwing an exception on error. See also 'tryVideoDriverName'.
videoDriverName :: IO String
videoDriverName = unwrapMaybe "SDL_VideoDriverName" tryVideoDriverName

foreign import ccall unsafe "SDL_GetVideoInfo" sdlGetVideoInfo :: IO (Ptr VideoInfoStruct)
getVideoInfo :: IO VideoInfo
getVideoInfo = sdlGetVideoInfo >>= newForeignPtr_

data ListModes
    = Modes [Rect] -- ^ List of available resolutions.
    | NonAvailable -- ^ No modes available!
    | AnyOK -- ^ All resolutions available.
      deriving (Show,Eq,Ord)

foreign import ccall unsafe "SDL_ListModes" sdlListModes :: Ptr PixelFormatStruct -> Word32 -> IO (Ptr (Ptr Rect))

-- | Returns the available screen resolutions for the given format and video flags.
listModes :: Maybe PixelFormat -- ^ Will use SDL_GetVideoInfo()->vfmt when @Nothing@.
          -> [SurfaceFlag]
          -> IO ListModes
listModes mbFormat flags
    = do ret <- getFormat (\ptr -> sdlListModes ptr (toBitmask flags))
         if ret == nullPtr `plusPtr` (-1)
            then return AnyOK
            else if ret == nullPtr
                    then return NonAvailable
                    else do array <- peekArray0 nullPtr ret
                            fmap Modes (mapM peek array)
    where getFormat = maybe (\action -> action nullPtr) withForeignPtr mbFormat

-- int SDL_VideoModeOK(int width, int height, int bpp, Uint32 flags);
foreign import ccall unsafe "SDL_VideoModeOK" sdlVideoModeOK :: CInt -> CInt -> CInt -> Word32 -> IO CInt

-- | Check to see if a particular video mode is supported.
--   Returns the bits-per-pixel of the closest available mode with the given width,
--   height and requested surface flags, or @Nothing@ on error.
videoModeOK :: Int -- ^ Width.
            -> Int -- ^ Height.
            -> Int -- ^ Bits-per-pixel.
            -> [SurfaceFlag] -- ^ Flags.
            -> IO (Maybe Int)
videoModeOK width height bpp flags
    = do ret <- sdlVideoModeOK (toCInt width) (toCInt height) (toCInt bpp) (toBitmask flags)
         case ret of
           0 -> return Nothing
           x -> return (Just $ fromCInt x)

-- SDL_Surface *SDL_SetVideoMode(int width, int height, int bpp, Uint32 flags);
foreign import ccall unsafe "SDL_SetVideoMode" sdlSetVideoMode :: CInt -> CInt -> CInt -> Word32 -> IO (Ptr SurfaceStruct)

-- | Set up a video mode with the specified width, height and bits-per-pixel.
--   Returns @Nothing@ on error.
trySetVideoMode :: Int -- ^ Width.
                -> Int -- ^ Height.
                -> Int -- ^ Bits-per-pixel.
                -> [SurfaceFlag] -- ^ Flags.
                -> IO (Maybe Surface)
trySetVideoMode width height bpp flags
    = sdlSetVideoMode (toCInt width) (toCInt height) (toCInt bpp) (toBitmask flags) >>= maybePeek newForeignPtr_

-- | Same as 'trySetVideoMode' except it throws an exception on error.
setVideoMode :: Int -> Int -> Int -> [SurfaceFlag] -> IO Surface
setVideoMode width height bpp flags
    = unwrapMaybe "SDL_SetVideoMode" (trySetVideoMode width height bpp flags)

-- void SDL_UpdateRect(SDL_Surface *screen, Sint32 x, Sint32 y, Sint32 w, Sint32 h);
foreign import ccall unsafe "SDL_UpdateRect" sdlUpdateRect :: Ptr SurfaceStruct
                                                           -> Int32 -> Int32 -> Word32 -> Word32 -> IO ()

-- | Makes sure the given area is updated on the given screen.
updateRect :: Surface -> Rect -> IO ()
updateRect surface rect
    = withForeignPtr surface (\ptr -> sdlUpdateRect ptr x y w h)
    where x = fromIntegral (rectX rect)
          y = fromIntegral (rectY rect)
          w = fromIntegral (rectW rect)
          h = fromIntegral (rectH rect)


-- void SDL_UpdateRects(SDL_Surface *screen, int numrects, SDL_Rect *rects);
foreign import ccall unsafe "SDL_UpdateRects" sdlUpdateRects :: Ptr SurfaceStruct -> CInt -> Ptr Rect -> IO ()

-- | Makes sure the given list of rectangles is updated on the given screen.
--   The rectangles are not automatically merged or checked for overlap.
--   In general, the programmer can use his knowledge about his particular
--   rectangles to merge them in an efficient way, to avoid overdraw.
updateRects :: Surface -> [Rect] -> IO ()
updateRects surface rects
    = withForeignPtr surface $ \ptr ->
      withArrayLen rects $ \len array ->
      sdlUpdateRects ptr (toCInt len) array

-- int SDL_Flip(SDL_Surface *screen);
foreign import ccall unsafe "SDL_Flip" sdlFlip :: Ptr SurfaceStruct -> IO CInt

-- | Swaps screen buffers.
tryFlip :: Surface -> IO Bool
tryFlip surface
    = withForeignPtr surface $ \ptr ->
      do ret <- sdlFlip ptr
         case ret of
           (-1) -> return False
           _    -> return True

-- | Same as 'tryFlip' but throws an exception on error.
flip :: Surface -> IO ()
flip = unwrapBool "SDL_Flip" . tryFlip

-- int SDL_SetColors(SDL_Surface *surface, SDL_Color *colors, int firstcolor, int ncolors);
foreign import ccall unsafe "SDL_SetColors" sdlSetColors :: Ptr SurfaceStruct -> Ptr Color -> CInt -> CInt -> IO CInt

-- | Sets a portion of the colormap for the given 8-bit surface.
setColors :: Surface -> [Color] -> Int -> IO Bool
setColors surface colors start
    = withForeignPtr surface $ \ptr ->
      withArrayLen colors $ \len array ->
      fmap toBool (sdlSetColors ptr array (toCInt start) (toCInt len))

-- int SDL_SetPalette(SDL_Surface *surface, int flags, SDL_Color *colors, int firstcolor, int ncolors);
foreign import ccall unsafe "SDL_SetPalette" sdlSetPalette
    :: Ptr SurfaceStruct -> CInt -> Ptr Color -> CInt -> CInt -> IO CInt

-- | Sets the colors in the palette of an 8-bit surface.
setPalette :: Surface -> [Palette] -> [Color] -> Int -> IO Bool
setPalette surface flags colors start
    = withForeignPtr surface $ \ptr ->
      withArrayLen colors $ \len array ->
      fmap toBool (sdlSetPalette ptr (toCInt $ toBitmask flags) array (toCInt start) (toCInt len))

--int SDL_SetGamma(float redgamma, float greengamma, float bluegamma);
foreign import ccall unsafe "SDL_SetGamma" sdlSetGamma :: Float -> Float -> Float -> IO CInt
setGamma :: Float -> Float -> Float -> IO Bool
setGamma red green blue
    = intToBool (-1) (fmap fromCInt $ sdlSetGamma red green blue)

-- int SDL_GetGammaRamp(Uint16 *redtable, Uint16 *greentable, Uint16 *bluetable);
foreign import ccall unsafe "SDL_GetGammaRamp" sdlGetGammaRamp :: Ptr Word16 -> Ptr Word16 -> Ptr Word16 -> IO CInt
tryGetGammaRamp :: IO (Maybe ([Word16],[Word16],[Word16]))
tryGetGammaRamp
    = allocaArray size $ \red ->
      allocaArray size $ \green ->
      allocaArray size $ \blue ->
      do ret <- sdlGetGammaRamp red green blue
         case ret of
           (-1) -> return Nothing
           _ -> do [r,g,b] <- mapM (peekArray size) [red,green,blue]
                   return (Just (r,g,b))
    where size = 256

getGammaRamp :: IO ([Word16],[Word16],[Word16])
getGammaRamp = unwrapMaybe "SDL_GetGammaRamp" tryGetGammaRamp

-- int SDL_SetGammaRamp(Uint16 *redtable, Uint16 *greentable, Uint16 *bluetable);
foreign import ccall unsafe "SDL_SetGammaRamp" sdlSetGammaRamp :: Ptr Word16 -> Ptr Word16 -> Ptr Word16 -> IO CInt
trySetGammaRamp :: [Word16] -> [Word16] -> [Word16] -> IO Bool
trySetGammaRamp red green blue
    = withArrayLen red $ check $ \ptrRed ->
      withArrayLen green $ check $ \ptrGreen ->
      withArrayLen blue $ check $ \ptrBlue ->
      intToBool (-1) (fmap fromCInt $ sdlSetGammaRamp ptrRed ptrGreen ptrBlue)
    where check action 256 ptr = action ptr
          check _ _ _ = return False

setGammaRamp :: [Word16] -> [Word16] -> [Word16] -> IO ()
setGammaRamp red green blue = unwrapBool "setGammaRamp_" (trySetGammaRamp red green blue)

-- Uint32 SDL_MapRGB(SDL_PixelFormat *fmt, Uint8 r, Uint8 g, Uint8 b);
foreign import ccall unsafe "SDL_MapRGB" sdlMapRGB :: Ptr PixelFormatStruct -> Word8 -> Word8 -> Word8 -> IO Word32

-- | Map a RGB color value to a pixel format.
mapRGB :: PixelFormat
       -> Word8 -- ^ Red value.
       -> Word8 -- ^ Green value.
       -> Word8 -- ^ Blue value.
       -> IO Pixel
mapRGB format r g b
    = withForeignPtr format $ \ptr ->
      fmap Pixel (sdlMapRGB ptr r g b)

-- Uint32 SDL_MapRGBA(SDL_PixelFormat *fmt, Uint8 r, Uint8 g, Uint8 b, Uint8 a);

foreign import ccall unsafe "SDL_MapRGBA" sdlMapRGBA
    :: Ptr PixelFormatStruct -> Word8 -> Word8 -> Word8 -> Word8 -> IO Word32

-- | Map a RGBA color value to a pixel format.
mapRGBA :: PixelFormat
        -> Word8 -- ^ Red value.
        -> Word8 -- ^ Green value.
        -> Word8 -- ^ Blue value.
        -> Word8 -- ^ Alpha value.
        -> IO Pixel
mapRGBA format r g b a
    = withForeignPtr format $ \ptr ->
      fmap Pixel (sdlMapRGBA ptr r g b a)

-- void SDL_GetRGB(Uint32 pixel, SDL_PixelFormat *fmt, Uint8 *r, Uint8 *g, Uint8 *b);
foreign import ccall unsafe "SDL_GetRGB" sdlGetRGB
    :: Word32 -> Ptr PixelFormatStruct -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO ()

-- | Get RGB values from a pixel in the specified pixel format.
getRGB :: Pixel -> PixelFormat -> IO (Word8,Word8,Word8)
getRGB (Pixel p) format
    = alloca $ \red ->
      alloca $ \green ->
      alloca $ \blue ->
      withForeignPtr format $ \ptr ->
      do sdlGetRGB p ptr red green blue
         [r,g,b] <- mapM peek [red,green,blue]
         return (r,g,b)

-- void SDL_GetRGBA(Uint32 pixel, SDL_PixelFormat *fmt, Uint8 *r, Uint8 *g, Uint8 *b, Uint8 *a);
foreign import ccall unsafe "SDL_GetRGBA" sdlGetRGBA
    :: Word32 -> Ptr PixelFormatStruct -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO ()

-- | Gets RGBA values from a pixel in the specified pixel format.
getRGBA :: Pixel -> PixelFormat -> IO (Word8,Word8,Word8,Word8)
getRGBA (Pixel p) format
    = alloca $ \red ->
      alloca $ \green ->
      alloca $ \blue ->
      alloca $ \alpha -> 
      withForeignPtr format $ \ptr ->
      do sdlGetRGBA p ptr red green blue alpha
         [r,g,b,a] <- mapM peek [red,green,blue,alpha]
         return (r,g,b,a)

-- SDL_Surface *SDL_CreateRGBSurface(Uint32 flags, int width, int height, int depth, Uint32 Rmask, Uint32 Gmask, Uint32 Bmask, Uint32 Amask);
foreign import ccall unsafe "SDL_CreateRGBSurface" sdlCreateRGBSurface
    :: Word32 -> CInt -> CInt -> CInt -> Word32 -> Word32 -> Word32 -> Word32 -> IO (Ptr SurfaceStruct)

-- | Creates an empty @Surface@. Returns @Nothing@ on error.
tryCreateRGBSurface :: [SurfaceFlag] -> Int -> Int -> Int
                  -> Word32 -> Word32 -> Word32 -> Word32 -> IO (Maybe Surface)
tryCreateRGBSurface flags width height bpp rmask gmask bmask amask
    = sdlCreateRGBSurface (toBitmask flags) (toCInt width) (toCInt height) (toCInt bpp) rmask gmask bmask amask >>=
      maybePeek mkFinalizedSurface

-- | Creates an empty @Surface@. Throws an exception on error.
createRGBSurface :: [SurfaceFlag] -> Int -> Int -> Int
                 -> Word32 -> Word32 -> Word32 -> Word32 -> IO Surface
createRGBSurface flags width height bpp rmask gmask bmask amask
    = unwrapMaybe "SDL_CreateRGBSurface" (tryCreateRGBSurface flags width height bpp rmask gmask bmask amask)

-- | Creates an empty @Surface@ with (r\/g\/b\/a)mask determined from the local endian.
--   Returns @Nothing@ on error.
tryCreateRGBSurfaceEndian :: [SurfaceFlag] -> Int -> Int -> Int -> IO (Maybe Surface)
tryCreateRGBSurfaceEndian flags width height bpp
    = tryCreateRGBSurface flags width height bpp
#if SDL_BYTEORDER == SDL_LIL_ENDIAN
        0x000000FF 0x0000FF00 0x00FF0000 0xFF000000
#else
        0xFF000000 0x00FF0000 0x0000FF00 0x000000FF
#endif

-- | Creates an empty @Surface@ with (r\/g\/b\/a)mask determined from the local endian.
--   Throws an exception on error.
createRGBSurfaceEndian :: [SurfaceFlag] -> Int -> Int -> Int -> IO Surface
createRGBSurfaceEndian flags width height bpp
    = unwrapMaybe "SDL_CreateRGBSurface" (tryCreateRGBSurfaceEndian flags width height bpp)

-- SDL_Surface *SDL_CreateRGBSurfaceFrom(void *pixels, int width, int height, int depth, int pitch, Uint32 Rmask, Uint32 Gmask, Uint32 Bmask, Uint32 Amask);
foreign import ccall unsafe "SDL_CreateRGBSurfaceFrom" sdlCreateRGBSurfaceFrom
    :: Ptr Word8 -> CInt -> CInt -> CInt -> CInt -> Word32 -> Word32 -> Word32 -> Word32 -> IO (Ptr SurfaceStruct)
tryCreateRGBSurfaceFrom :: Ptr a -> Int -> Int -> Int -> Int
                        -> Word32 -> Word32 -> Word32 -> Word32 -> IO (Maybe Surface)
tryCreateRGBSurfaceFrom pixels width height depth pitch rmask gmask bmask amask
    = sdlCreateRGBSurfaceFrom (castPtr pixels) (toCInt width) (toCInt height) (toCInt depth) (toCInt pitch) rmask gmask bmask amask >>=
      maybePeek mkFinalizedSurface

createRGBSurfaceFrom :: Ptr a -> Int -> Int -> Int -> Int
                     -> Word32 -> Word32 -> Word32 -> Word32 -> IO Surface
createRGBSurfaceFrom pixels width height depth pitch rmask gmask bmask amask
    = unwrapMaybe "SDL_CreateRGBSurfaceFrom"
                  (tryCreateRGBSurfaceFrom pixels width height depth pitch rmask gmask bmask amask)

{-
-- void SDL_FreeSurface(SDL_Surface *surface);
foreign import ccall unsafe "SDL_FreeSurface" sdlFreeSurface :: Ptr SurfaceStruct -> IO ()
-- | Frees (deletes) a @Surface@. Don\'t use it unless you really know what you're doing. All surfaces
--   are automatically deleted when they're out of scope or forced with @finalizeForeignPtr@.
freeSurface :: Surface -> IO ()
freeSurface surface
    = withForeignPtr surface sdlFreeSurface
-}

-- | Forces the finalization of a @Surface@. Only supported with GHC.
freeSurface :: Surface -> IO ()
freeSurface =
#if defined(__GLASGOW_HASKELL__)
  finalizeForeignPtr
#else
  const (return ())
#endif

-- int SDL_LockSurface(SDL_Surface *surface);
foreign import ccall unsafe "SDL_LockSurface" sdlLockSurface :: Ptr SurfaceStruct -> IO CInt

-- | Locks a surface for direct access.
lockSurface :: Surface -> IO Bool
lockSurface surface
    = withForeignPtr surface $ \ptr ->
      intToBool (-1) (fmap fromCInt $ sdlLockSurface ptr)

-- void SDL_UnlockSurface(SDL_Surface *surface);
foreign import ccall unsafe "SDL_UnlockSurface" sdlUnlockSurface :: Ptr SurfaceStruct -> IO ()

-- | Unlocks a previously locked surface.
unlockSurface :: Surface -> IO ()
unlockSurface surface = withForeignPtr surface sdlUnlockSurface

-- extern DECLSPEC SDL_Surface * SDLCALL SDL_LoadBMP_RW(SDL_RWops *src, int freesrc);
-- define SDL_LoadBMP(file)       SDL_LoadBMP_RW(SDL_RWFromFile(file, "rb"), 1)
foreign import ccall unsafe "SDL_LoadBMP_RW" sdlLoadBMP_RW :: Ptr RWopsStruct -> CInt -> IO (Ptr SurfaceStruct)

tryLoadBMPRW :: RWops -> Bool -> IO (Maybe Surface)
tryLoadBMPRW rw freesrc
    = withForeignPtr rw $ \rwPtr ->
      sdlLoadBMP_RW rwPtr (fromBool freesrc) >>= maybePeek mkFinalizedSurface

loadBMPRW :: RWops -> Bool -> IO Surface
loadBMPRW rw freesrc = unwrapMaybe "SDL_LoadBMP_RW" (tryLoadBMPRW rw freesrc)

loadBMP :: FilePath -> IO Surface
loadBMP filepath
    = RW.with filepath "rb" $ \rw ->
      loadBMPRW rw False

-- extern DECLSPEC int SDLCALL SDL_SaveBMP_RW
--                (SDL_Surface *surface, SDL_RWops *dst, int freedst);
foreign import ccall unsafe "SDL_SaveBMP_RW" sdlSaveBMP_RW :: Ptr SurfaceStruct -> Ptr RWopsStruct -> CInt -> IO CInt

saveBMPRW :: Surface -> RWops -> Bool -> IO Bool
saveBMPRW surface rw freedst
    = withForeignPtr surface $ \ptr ->
      withForeignPtr rw $ \rwPtr ->
      intToBool (-1) (fmap fromCInt $ sdlSaveBMP_RW ptr rwPtr (fromBool freedst))

saveBMP :: Surface -> FilePath -> IO Bool
saveBMP surface filepath
    = RW.with filepath "wb" $ \rw ->
      saveBMPRW surface rw False


-- int SDL_SetColorKey(SDL_Surface *surface, Uint32 flag, Uint32 key);
foreign import ccall unsafe "SDL_SetColorKey" sdlSetColorKey :: Ptr SurfaceStruct -> Word32 -> Word32 -> IO CInt
setColorKey :: Surface -> [SurfaceFlag] -> Pixel -> IO Bool
setColorKey surface flags (Pixel w)
    = withForeignPtr surface $ \ptr ->
      intToBool (-1) (fmap fromCInt $ sdlSetColorKey ptr (toBitmask flags) w)

-- int SDL_SetAlpha(SDL_Surface *surface, Uint32 flag, Uint8 alpha);
foreign import ccall unsafe "SDL_SetAlpha" sdlSetAlpha :: Ptr SurfaceStruct -> Word32 -> Word8 -> IO CInt

-- | Adjusts the alpha properties of a surface.
setAlpha :: Surface -> [SurfaceFlag] -> Word8 -> IO Bool
setAlpha surface flags alpha
    = withForeignPtr surface $ \ptr ->
      intToBool (-1) (fmap fromCInt $ sdlSetAlpha ptr (toBitmask flags) alpha)

-- void SDL_SetClipRect(SDL_Surface *surface, SDL_Rect *rect);
foreign import ccall unsafe "SDL_SetClipRect" sdlSetClipRect :: Ptr SurfaceStruct -> Ptr Rect -> IO ()

-- | Sets the clipping rectangle for a surface.
setClipRect :: Surface -> Maybe Rect -> IO ()
setClipRect surface mbRect
    = withForeignPtr surface $ \ptr ->
      maybeWith with mbRect $ \rect ->
      sdlSetClipRect ptr rect

-- void SDL_GetClipRect(SDL_Surface *surface, SDL_Rect *rect);
foreign import ccall unsafe "SDL_GetClipRect" sdlGetClipRect :: Ptr SurfaceStruct -> Ptr Rect -> IO ()

-- | Gets the clipping rectangle for a surface.
getClipRect :: Surface -> IO Rect
getClipRect surface
    = withForeignPtr surface $ \ptr ->
      alloca $ \rectPtr ->
      do sdlGetClipRect ptr rectPtr
         peek rectPtr

-- | Run an action with a given clipping rect applied.
--   If an exception is raised, then withClipRect will re-raise the exception (after resetting the original clipping rect).
withClipRect :: Surface -> Maybe Rect -> IO a -> IO a
withClipRect surface rect action
    = bracket (getClipRect surface) -- Get the current cliprect
              (setClipRect surface . Just) -- Reset to old cliprect when done.
              (const (setClipRect surface rect >> action)) -- Set new cliprect.

-- SDL_Surface *SDL_ConvertSurface(SDL_Surface *src, SDL_PixelFormat *fmt, Uint32 flags);
foreign import ccall unsafe "SDL_ConvertSurface" sdlConvertSurface
    :: Ptr SurfaceStruct -> Ptr PixelFormatStruct -> Word32 -> IO (Ptr SurfaceStruct)

-- | Converts a surface to the same format as another surface. Returns @Nothing@ on error.
tryConvertSurface :: Surface -> PixelFormat -> [SurfaceFlag] -> IO (Maybe Surface)
tryConvertSurface surface format flags
    = withForeignPtr surface $ \ptr ->
      withForeignPtr format $ \formatPtr ->
      sdlConvertSurface ptr formatPtr (toBitmask flags) >>= maybePeek mkFinalizedSurface

-- | Converts a surface to the same format as another surface. Throws an exception on error.
convertSurface :: Surface -> PixelFormat -> [SurfaceFlag] -> IO Surface
convertSurface surface format flags
    = unwrapMaybe "SDL_ConvertSurface"
                  (tryConvertSurface surface format flags)


-- int SDL_UpperBlit(SDL_Surface *src, SDL_Rect *srcrect, SDL_Surface *dst, SDL_Rect *dstrect);
foreign import ccall unsafe "SDL_UpperBlit" sdlBlitSurface
    :: Ptr SurfaceStruct -> Ptr Rect -> Ptr SurfaceStruct -> Ptr Rect -> IO CInt

-- | This function performs a fast blit from the source surface to the destination surface.
blitSurface :: Surface -> Maybe Rect -> Surface -> Maybe Rect -> IO Bool
blitSurface src srcRect dst dstRect
    = withForeignPtr src $ \srcPtr ->
      withForeignPtr dst $ \dstPtr ->
      maybeWith with srcRect $ \srcRectPtr ->
      maybeWith with dstRect $ \dstRectPtr ->
      intToBool (-1) (fmap fromCInt $ sdlBlitSurface srcPtr srcRectPtr dstPtr dstRectPtr)


-- int SDL_FillRect(SDL_Surface *dst, SDL_Rect *dstrect, Uint32 color);
foreign import ccall unsafe "SDL_FillRect" sdlFillRect :: Ptr SurfaceStruct -> Ptr Rect -> Word32 -> IO CInt

-- | This function performs a fast fill of the given rectangle with some color.
fillRect :: Surface -> Maybe Rect -> Pixel -> IO Bool
fillRect surface mbRect (Pixel w)
    = withForeignPtr surface $ \ptr ->
      maybeWith with mbRect $ \rect ->
      intToBool (-1) (fmap fromCInt $ sdlFillRect ptr rect w)

-- SDL_Surface *SDL_DisplayFormat(SDL_Surface *surface);
foreign import ccall unsafe "SDL_DisplayFormat" sdlDisplayFormat :: Ptr SurfaceStruct -> IO (Ptr SurfaceStruct)

-- | Converts a surface to the display format. Returns @Nothing@ on error.
tryDisplayFormat :: Surface -> IO (Maybe Surface)
tryDisplayFormat surface
    = withForeignPtr surface $ \ptr ->
      sdlDisplayFormat ptr >>= maybePeek mkFinalizedSurface

-- | Converts a surface to the display format. Throws an exception on error.
displayFormat :: Surface -> IO Surface
displayFormat = unwrapMaybe "SDL_DisplayFormat" . tryDisplayFormat

-- SDL_Surface *SDL_DisplayFormatAlpha(SDL_Surface *surface);
foreign import ccall unsafe "SDL_DisplayFormatAlpha" sdlDisplayFormatAlpha :: Ptr SurfaceStruct -> IO (Ptr SurfaceStruct)

-- | Converts a surface to the display format. Returns @Nothing@ on error.
tryDisplayFormatAlpha :: Surface -> IO (Maybe Surface)
tryDisplayFormatAlpha surface
    = withForeignPtr surface $ \ptr ->
      sdlDisplayFormatAlpha ptr >>= maybePeek mkFinalizedSurface

-- | Converts a surface to the display format. Throws an exception on error.
displayFormatAlpha :: Surface -> IO Surface
displayFormatAlpha = unwrapMaybe "SDL_DisplayFormatAlpha" . tryDisplayFormatAlpha

-- void SDL_WarpMouse(Uint16 x, Uint16 y);
foreign import ccall unsafe "SDL_WarpMouse" sdlWarpMouse :: Word16 -> Word16 -> IO ()

-- | Sets the position of the mouse cursor.
warpMouse :: Word16 -- ^ Mouse X position.
          -> Word16 -- ^ Mouse Y position.
          -> IO ()
warpMouse = sdlWarpMouse

-- int SDL_ShowCursor(int toggle);
foreign import ccall unsafe "SDL_ShowCursor" sdlShowCursor :: CInt -> IO CInt

-- | Toggle whether or not the cursor is shown on the screen.
showCursor :: Bool -> IO ()
showCursor enable
    = sdlShowCursor (fromToggle toggle) >>
      return ()
    where toggle = case enable of
                     True -> Enable
                     False -> Disable

-- | Returns @True@ when the cursor is set to visible. See also 'showCursor'.
queryCursorState :: IO Bool
queryCursorState = fmap toBool (sdlShowCursor (fromToggle Query))


type GLAttr = CInt
type GLValue = CInt

glRedSize, glGreenSize, glBlueSize, glAlphaSize, glBufferSize, glDoubleBuffer :: GLAttr
glDepthSize, glStencilSize, glAccumRedSize, glAccumGreenSize, glAccumBlueSize :: GLAttr
glAccumAlphaSize, glStereo, glMultiSampleBuffers, glMultiSampleSamples :: GLAttr

glRedSize = #{const SDL_GL_RED_SIZE}
glGreenSize = #{const SDL_GL_GREEN_SIZE}
glBlueSize = #{const SDL_GL_BLUE_SIZE}
glAlphaSize = #{const SDL_GL_ALPHA_SIZE}
glBufferSize = #{const SDL_GL_BUFFER_SIZE}
glDoubleBuffer = #{const SDL_GL_DOUBLEBUFFER}
glDepthSize  = #{const SDL_GL_DEPTH_SIZE}
glStencilSize = #{const SDL_GL_STENCIL_SIZE}
glAccumRedSize = #{const SDL_GL_ACCUM_RED_SIZE}
glAccumGreenSize = #{const SDL_GL_ACCUM_GREEN_SIZE}
glAccumBlueSize = #{const SDL_GL_ACCUM_BLUE_SIZE}
glAccumAlphaSize = #{const SDL_GL_ACCUM_ALPHA_SIZE}
glStereo = #{const SDL_GL_STEREO}
glMultiSampleBuffers = #{const SDL_GL_MULTISAMPLEBUFFERS}
glMultiSampleSamples = #{const SDL_GL_MULTISAMPLESAMPLES}

--int SDL_GL_SetAttribute(SDL_GLattr attr, int value);
foreign import ccall unsafe "SDL_GL_SetAttribute" sdlGLSetAttribute :: CInt -> CInt -> IO CInt
-- | Sets a special SDL\/OpenGL attribute. Returns @False@ on error.
tryGLSetAttribute :: GLAttr -> GLValue -> IO Bool
tryGLSetAttribute attr value = fmap (==0) (sdlGLSetAttribute attr value)

-- | Sets a special SDL\/OpenGL attribute. Throws an exception on error.
glSetAttribute :: GLAttr -> GLValue -> IO ()
glSetAttribute attr value = unwrapBool "SDL_GL_SetAttribute" (tryGLSetAttribute attr value)

-- int SDL_GL_GetAttribute(SDLGLattr attr, int *value);
foreign import ccall unsafe "SDL_GL_GetAttribute" sdlGLGetAttribute :: CInt -> Ptr CInt -> IO CInt

-- | Gets the value of a special SDL\/OpenGL attribute. Returns @Nothing@ on error.
tryGLGetAttribute :: GLAttr -> IO (Maybe GLValue)
tryGLGetAttribute attr
    = alloca $ \valuePtr ->
      do ret <- sdlGLGetAttribute attr valuePtr
         case ret of
           0 -> fmap Just (peek valuePtr)
           _ -> return Nothing

-- | Gets the value of a special SDL\/OpenGL attribute. Throws an exception on error.
glGetAttribute :: GLAttr -> IO GLValue
glGetAttribute = unwrapMaybe "SDL_GL_GetAttribute" . tryGLGetAttribute

--void SDLCALL SDL_GL_SwapBuffers(void);
-- | Swaps OpenGL framebuffers\/Update Display.
foreign import ccall unsafe "SDL_GL_SwapBuffers" glSwapBuffers :: IO ()

foreign import ccall unsafe "&SDL_FreeSurface" sdlFreeSurfaceFinal :: FunPtr (Ptr SurfaceStruct -> IO ())

mkFinalizedSurface :: Ptr SurfaceStruct -> IO Surface
mkFinalizedSurface = newForeignPtr sdlFreeSurfaceFinal

