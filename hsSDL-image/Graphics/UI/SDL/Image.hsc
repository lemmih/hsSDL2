#include "SDL_image.h"
module Graphics.UI.SDL.Image
  ( -- * Library initialization.
    withImageLibrary
  , initialize
  , quit
    -- * Image loading.
  , load
  ) where

import Foreign
import Foreign.C
import Control.Monad
import Control.Exception

import Graphics.UI.SDL             ( failWithError )
import Graphics.UI.SDL.Video
import Graphics.UI.SDL.Types
import Graphics.UI.SDL.Utilities

data ImageFlag
  = InitJPG
  | InitPNG
  | InitTIF
  deriving ( Eq, Ord, Show, Read, Bounded, Enum )

imageFlagToC :: ImageFlag -> CInt
imageFlagToC InitJPG = #{const IMG_INIT_JPG}
imageFlagToC InitPNG = #{const IMG_INIT_PNG}
imageFlagToC InitTIF = #{const IMG_INIT_TIF}

{-
int IMG_Init(int flags)

flags
bitwise OR'd set of image formats to support by loading a library now. The values you may OR together to pass in are:
IMG_INIT_JPG
IMG_INIT_PNG
IMG_INIT_TIF
Initialize by loading support as indicated by the flags, or at least return success if support is already loaded. You may call this multiple times, which will actually require you to call IMG_Quit just once to clean up. You may call this function with a 0 to retrieve whether support was built-in or not loaded yet.
Note: to load JPG, PNG, and/or TIF images you can call IMG_Init with the right IMG_INIT_* flags OR'd together before you program gets busy, to prevent a later hiccup while it loads the library, and to check that you do have the support that you need before you try and use it.
Note: No initialization is needed nor performed when using the IMG_isJPG, IMG_isPNG, and IMG_isTIF functions.
Note: this function does not always set the error string, so do not depend on IMG_GetError being meaningful all the time.
-}

foreign import ccall unsafe "IMG_Init"
  imgInit :: CInt -> IO CInt

initialize :: [ImageFlag] -> IO ()
initialize flags = do
  ret <- imgInit (toBitmask imageFlagToC flags)
  when (ret == (-1)) (failWithError "IMG_Init")

withImageLibrary :: [ImageFlag] -> IO a -> IO a
withImageLibrary flags = bracket_ (initialize flags) quit

{-
void IMG_Quit()

This function cleans up all dynamically loaded library handles, freeing memory. If support is
required again it will be initialized again, either by IMG_Init or loading an image with
dynamic support required. You may call this function when IMG_Load functions are no longer
needed for the JPG, PNG, and TIF image formats. You only need to call this function once, no
matter how many times IMG_Init was called.
-}
foreign import ccall unsafe "IMG_Quit" quit :: IO ()




{-
SDL_Surface *IMG_Load(const char *file)

file
Image file name to load a surface from.
Load file for use as an image in a new surface. This actually calls IMG_LoadTyped_RW, with the file extension used as the type string. This can load all supported image files, including TGA as long as the filename ends with ".tga". It is best to call this outside of event loops, and rather keep the loaded images around until you are really done with them, as disk speed and image conversion to a surface is not that speedy. Don't forget to SDL_FreeSurface the returned surface pointer when you are through with it.
Note: If the image format loader requires initialization, it will attempt to do that the first time it is needed if you have not already called IMG_Init to load support for your image format.
Note: If the image format supports a transparent pixel, SDL_image will set the colorkey for the surface. You can enable RLE acceleration on the surface afterwards by calling:
SDL_SetColorKey(image, SDL_RLEACCEL, image->format->colorkey);

Returns: a pointer to the image as a new SDL_Surface. NULL is returned on errors, such as no support built for the image, or a file reading error.
-}
foreign import ccall unsafe "IMG_Load"
  imgLoad :: CString -> IO (Ptr SurfaceStruct)

-- FIXME: Error handling
load :: FilePath -> IO Surface
load filepath =
  withCString filepath $ \cPath -> do
    surface <- imgLoad cPath
    mkFinalizedSurface surface



