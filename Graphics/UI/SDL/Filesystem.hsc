#include "SDL.h"
module Graphics.UI.SDL.Filesystem
  ( getBasePath
  , getPrefPath
  ) where

import Foreign
import Foreign.C.String

foreign import ccall unsafe "SDL_GetBasePath"
  getBasePath :: IO CString


foreign import ccall unsafe "SDL_GetPrefPath"
  getPrefPath:: CString -> CString -> IO CString

