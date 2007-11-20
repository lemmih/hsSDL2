#include "SDL.h"
#ifdef main
#undef main
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.SDL.WindowManagement
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Graphics.UI.SDL.WindowManagement
    ( GrabMode (..)
    , setCaption
    , rawSetCaption
    , getCaption
    , iconifyWindow
    , tryToggleFullscreen
    , toggleFullscreen
    , grabInput
    , queryGrabMode
    ) where

import Foreign (Int32, Ptr, Storable(peek), nullPtr, toBool, maybePeek,
                void, alloca, withForeignPtr)
import Foreign.C (withCString, peekCString, CString)

import Graphics.UI.SDL.Types (Surface, SurfaceStruct)
import Graphics.UI.SDL.General (unwrapBool)


data GrabMode
    = GrabQuery
    | GrabOff
    | GrabOn
      deriving (Show,Eq)

toGrabMode :: #{type SDL_GrabMode} -> GrabMode
toGrabMode (#{const SDL_GRAB_QUERY}) = GrabQuery
toGrabMode (#{const SDL_GRAB_OFF}) = GrabOff
toGrabMode (#{const SDL_GRAB_ON}) = GrabOn
toGrabMode _ = error "Graphics.UI.SDL.WindowManagement.toGrabMode: bad argument"

fromGrabMode :: GrabMode -> #{type SDL_GrabMode}
fromGrabMode GrabQuery = (#{const SDL_GRAB_QUERY})
fromGrabMode GrabOff = (#{const SDL_GRAB_OFF})
fromGrabMode GrabOn = (#{const SDL_GRAB_ON})

-- void SDL_WM_SetCaption(const char *title, const char *icon);
foreign import ccall unsafe "SDL_WM_SetCaption" sdlSetCaption :: CString -> CString -> IO ()
-- | Sets the window title and icon name.
setCaption :: String -> String -> IO ()
setCaption title icon
    = withCString title $ \titlePtr ->
      withCString icon $ \iconPtr ->
      sdlSetCaption titlePtr iconPtr

-- | Sets the window title and icon name. Use @Nothing@ to unset.
rawSetCaption :: Maybe String -> Maybe String -> IO ()
rawSetCaption title icon
    = maybeStr title $ \titlePtr ->
      maybeStr icon $ \iconPtr ->
      sdlSetCaption titlePtr iconPtr
    where maybeStr Nothing action = action nullPtr
          maybeStr (Just s) action = withCString s action
-- void SDL_WM_GetCaption(char **title, char **icon);
foreign import ccall unsafe "SDL_WM_GetCaption" sdlGetCaption :: Ptr CString -> Ptr CString -> IO ()
-- | Gets the window title and icon name.
getCaption :: IO (Maybe String,Maybe String)
getCaption
    = alloca $ \cTitle ->
      alloca $ \cIcon ->
      do sdlGetCaption cTitle cIcon
         title <- maybePeek ((peekCString =<<).peek) cTitle
         icon <- maybePeek ((peekCString =<<).peek) cIcon
         return (title,icon)

-- int SDL_WM_IconifyWindow(void);
foreign import ccall unsafe "SDL_WM_IconifyWindow" sdlIconifyWindow :: IO Int
-- | Iconify\/Minimise the window.
iconifyWindow :: IO Bool
iconifyWindow = fmap toBool sdlIconifyWindow

-- int SDL_WM_ToggleFullScreen(SDL_Surface *surface);
foreign import ccall unsafe "SDL_WM_ToggleFullScreen" sdlToggleFullScreen :: Ptr SurfaceStruct -> IO Int
-- |Toggles fullscreen mode. Returns @False@ on error.
tryToggleFullscreen :: Surface -> IO Bool
tryToggleFullscreen surface
    = withForeignPtr surface $ fmap toBool . sdlToggleFullScreen

-- | Toggles fullscreen mode. Throws an exception on error.
toggleFullscreen :: Surface -> IO ()
toggleFullscreen = unwrapBool "SDL_WM_ToggleFullScreen" . tryToggleFullscreen

-- SDL_GrabMode SDL_WM_GrabInput(SDL_GrabMode mode);
foreign import ccall unsafe "SDL_WM_GrabInput" sdlGrabInput :: #{type SDL_GrabMode} -> IO #{type SDL_GrabMode}
-- | Grabbing means that the mouse is confined to the application
--   window, and nearly all keyboard input is passed directly to
--   the application, and not interpreted by a window manager, if any.
grabInput :: Bool -> IO ()
grabInput = void . sdlGrabInput . fromGrabMode . mkGrabMode
    where mkGrabMode True = GrabOn
          mkGrabMode False = GrabOff

-- | Returns the current grabbing mode.
queryGrabMode :: IO GrabMode
queryGrabMode = fmap toGrabMode . sdlGrabInput . fromGrabMode $ GrabQuery


