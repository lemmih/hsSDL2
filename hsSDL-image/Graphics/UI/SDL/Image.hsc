#include "SDL_image.h"


module Graphics.UI.SDL.Image
  ( -- * Library initialization.
    ImageFlag(..)
  , withInit
  , init
  , quit
    -- * Image loading.
  , load
  ) where


import           Control.Exception
import           Control.Monad
import           Foreign
import           Foreign.C
import           Prelude                   hiding (init)

import           Graphics.UI.SDL           (failWithError)
import           Graphics.UI.SDL.General   (handleError)
import           Graphics.UI.SDL.Error     (getError)
import           Graphics.UI.SDL.Types
import           Graphics.UI.SDL.Utilities (fromBitmask, toBitmask)
import           Graphics.UI.SDL.Video
import           Graphics.UI.SDL.Surface   (mkFinalizedSurface)


data ImageFlag
  = InitJPG
  | InitPNG
  | InitTIF
  deriving ( Eq, Ord, Show, Read, Bounded )

imageFlagToC :: ImageFlag -> CInt
imageFlagToC InitJPG = #{const IMG_INIT_JPG}
imageFlagToC InitPNG = #{const IMG_INIT_PNG}
imageFlagToC InitTIF = #{const IMG_INIT_TIF}

instance Enum ImageFlag where
    fromEnum InitJPG = #{const IMG_INIT_JPG}
    fromEnum InitPNG = #{const IMG_INIT_PNG}
    fromEnum InitTIF = #{const IMG_INIT_TIF}

    toEnum #{const IMG_INIT_JPG} = InitJPG
    toEnum #{const IMG_INIT_PNG} = InitPNG
    toEnum #{const IMG_INIT_TIF} = InitTIF
    toEnum i = error $ "ImageFlag.toEnum: Invalid argument: " ++ show i


foreign import ccall unsafe "IMG_Init"
  imgInit :: CInt -> IO CInt

init :: [ImageFlag] -> IO ()
init flags = do
    ret <- imgInit bitmask
    if ret .&. bitmask /= bitmask
      then (\err -> error $ "init: " ++ show err) =<< getError
      else return ()
  where
    bitmask = toBitmask imageFlagToC flags

withInit :: [ImageFlag] -> IO a -> IO a
withInit flags = bracket_ (init flags) quit

foreign import ccall unsafe "IMG_Quit" quit :: IO ()

foreign import ccall unsafe "IMG_Load"
  imgLoad :: CString -> IO (Ptr SurfaceStruct)

load :: FilePath -> IO Surface
load filepath =
  withCString filepath $ \cPath -> do
    surface <- imgLoad cPath
    handleError "load" surface mkFinalizedSurface
