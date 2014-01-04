#include "SDL.h"
module Graphics.UI.SDL.Error
    ( -- * SDL Routines
      clearError
    , getError
    , setError
    )  where

import Control.Applicative
import Control.Monad (mfilter, void)
import Foreign hiding (void)
import Foreign.C

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_ClearError"
  clearError :: IO ()
  
--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_GetError"
  sdlGetError :: IO CString
  
getError :: IO (Maybe String)
getError = mfilter (not . null) <$> (sdlGetError >>= maybePeek peekCString)

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_SetError"
  sdlSetError :: CString -> IO #{type int}

setError :: String -> IO ()
setError errorMessage = void $ withCString (escape errorMessage) sdlSetError
  where escape s = flip concatMap s $ \c -> case c of
                                              '%' -> "%%"
                                              _   -> [c]
