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
module Graphics.UI.SDL.WindowManagement where

import Foreign
import Foreign.C

import Graphics.UI.SDL.Utilities
import Graphics.UI.SDL.Types


data SDLGrabMode
    = SDLGrabQuery
    | SDLGrabOff
    | SDLGrabOn

instance Enum SDLGrabMode where
    toEnum (-1) = SDLGrabQuery
    toEnum 0 = SDLGrabOff
    toEnum 1 = SDLGrabOn
    fromEnum SDLGrabQuery = (-1)
    fromEnum SDLGrabOff = 0
    fromEnum SDLGrabOn = 1

-- void SDL_WM_SetCaption(const char *title, const char *icon);
foreign import ccall unsafe "SDL_WM_SetCaption" sdlSetCaption :: CString -> CString -> IO ()
setCaption :: String -> String -> IO ()
setCaption title icon
    = withCString title $ \titlePtr ->
      withCString icon $ \iconPtr ->
      sdlSetCaption titlePtr iconPtr

rawSetCaption :: Maybe String -> Maybe String -> IO ()
rawSetCaption title icon
    = maybeStr title $ \titlePtr ->
      maybeStr icon $ \iconPtr ->
      sdlSetCaption titlePtr iconPtr
    where maybeStr Nothing action = action nullPtr
          maybeStr (Just s) action = withCString s action
-- void SDL_WM_GetCaption(char **title, char **icon);
foreign import ccall unsafe "SDL_WM_GetCaption" sdlGetCaption :: Ptr CString -> Ptr CString -> IO ()
getCaption :: IO (Maybe String,Maybe String)
getCaption
    = alloca $ \cTitle ->
      alloca $ \cIcon ->
      do sdlGetCaption cTitle cIcon
         title <- if cTitle == nullPtr
                     then return Nothing
                     else fmap Just (peekCString =<< peek cTitle)
         icon <- if cIcon == nullPtr
                    then return Nothing
                    else fmap Just (peekCString =<< peek cIcon)
         return (title,icon)

-- int SDL_WM_IconifyWindow(void);
foreign import ccall unsafe "SDL_WM_IconifyWindow" sdlIconifyWindow :: IO Int
iconifyWindow :: IO Bool
iconifyWindow = fmap toBool sdlIconifyWindow

-- int SDL_WM_ToggleFullScreen(SDL_Surface *surface);
foreign import ccall unsafe "SDL_WM_ToggleFullScreen" sdlToggleFullScreen :: Ptr SurfaceStruct -> IO Int
toggleFullScreen :: Surface -> IO Bool
toggleFullScreen surface
    = withForeignPtr surface $ fmap toBool . sdlToggleFullScreen

-- SDL_GrabMode SDL_WM_GrabInput(SDL_GrabMode mode);
foreign import ccall unsafe "SDL_WM_GrabInput" sdlGrabInput :: Int -> IO Int
grabInput :: SDLGrabMode -> IO SDLGrabMode
grabInput = fmap toEnum . sdlGrabInput . fromEnum

