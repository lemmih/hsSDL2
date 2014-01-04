#include "SDL.h"
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
  ( -- * Window management
    withWindow
  , createWindow
  , destroyWindow

  , showWindow
  , hideWindow
  , maximizeWindow
  , minimizeWindow
  , raiseWindow
  , restoreWindow

  , setWindowBrightness
  , getWindowBrightness

  , setWindowGrab
  , getWindowGrab

  , setWindowMaximumSize
  , getWindowMaximumSize

  , setWindowMinimumSize
  , getWindowMinimumSize

  , setWindowPosition
  , getWindowPosition

  , setWindowSize
  , getWindowSize

  , getWindowPixelFormat

    -- * OpenGL
  , withOpenGL
  , glSwapWindow
  , glBindTexture
  , glUnbindTexture
  , withBoundTexture

    -- * Surfaces
  , loadBMP
  , freeSurface
  , setColorKey
  , surfaceFormat

    -- * Screensaver handling
  , disableScreenSaver
  , enableScreenSaver
  , withoutScreenSaver
  , isScreenSaverEnabled

    -- * Clipboard handling
  , getClipboardText
  , setClipboardText
  , hasClipboardText

    -- * Misc
  , mkFinalizedSurface

    -- * Pixel formats
  , allocFormat
  , mapRGBA
  ) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Foreign.C.Types
import Foreign.C
import Foreign
import Control.Exception                     ( bracket, bracket_ )
import Data.Text.Encoding
import qualified Data.Text as T
import Data.Text                             ( Text )
import Data.ByteString                       ( useAsCString, packCString )

import Graphics.UI.SDL.Rect
import Graphics.UI.SDL.Types
import Graphics.UI.SDL.Utilities             ( toBitmask )
import Graphics.UI.SDL.General

import qualified Graphics.UI.SDL.RWOps as RWOps

{-
SDL_Window* SDL_CreateWindow(const char* title,
                             int         x,
                             int         y,
                             int         w,
                             int         h,
                             Uint32      flags)
-}
foreign import ccall unsafe "SDL_CreateWindow"
  sdlCreateWindow :: CString -> CInt -> CInt -> CInt -> CInt -> CUInt -> IO (Ptr WindowStruct)

-- XXX: Will SDL2 always copy the given cstring?
withUtf8CString :: String -> (CString -> IO a) -> IO a
withUtf8CString = useAsCString . encodeUtf8 . T.pack

-- FIXME: Support flags.
createWindow :: String -> Position -> Size -> [WindowFlag] -> IO Window
createWindow title (Position x y) (Size w h) flags =
  withUtf8CString title $ \cstr -> do
    window <- sdlCreateWindow
                  cstr (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)
                  (toBitmask windowFlagToC flags)
    newForeignPtr sdlDestroyWindow_finalizer window

withWindow :: String -> Position -> Size -> [WindowFlag] -> (Window -> IO r) -> IO r
withWindow title position size flags action =
  bracket (createWindow title position size flags) destroyWindow action

-- void SDL_DestroyWindow(SDL_Window* window)

foreign import ccall unsafe "&SDL_DestroyWindow"
  sdlDestroyWindow_finalizer :: FunPtr (Ptr WindowStruct -> IO ())

destroyWindow :: Window -> IO ()
destroyWindow = finalizeForeignPtr

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_GL_CreateContext"
  sdlGlCreateContext :: Ptr WindowStruct -> IO (Ptr GLContextStruct)

foreign import ccall unsafe "SDL_GL_DeleteContext"
  sdlGlDeleteContext :: Ptr GLContextStruct -> IO ()

withOpenGL :: Window -> IO a -> IO a
withOpenGL w a = withForeignPtr w $ \win ->
  bracket (sdlGlCreateContext win) sdlGlDeleteContext (const a)

foreign import ccall unsafe "SDL_GL_SwapWindow"
  sdlGlSwapWindow :: Ptr WindowStruct -> IO ()

glSwapWindow :: Window -> IO ()
glSwapWindow w = withForeignPtr w sdlGlSwapWindow

foreign import ccall unsafe "SDL_GL_BindTexture"
  sdlGlBindTexture :: Ptr TextureStruct -> Ptr CFloat -> Ptr CFloat -> IO CInt

-- | Bind a texture to the active texture unit in the current OpenGL context.
glBindTexture :: Texture -> IO ()
glBindTexture tex = Control.Monad.void $ withForeignPtr tex $ \texp ->
  sdlGlBindTexture texp nullPtr nullPtr

foreign import ccall unsafe "SDL_GL_UnbindTexture"
  sdlGlUnbindTexture :: Ptr TextureStruct -> IO CInt

-- | Unbind a texture from the current OpenGL context.
glUnbindTexture :: Texture -> IO ()
glUnbindTexture tex = Control.Monad.void $ withForeignPtr tex $ \texp ->
  sdlGlUnbindTexture texp

-- | Run an action with a texture bound to the active texture unit in the current OpenGL context, and unbind it afterwards.
withBoundTexture :: Texture -> IO a -> IO a
withBoundTexture tex = bracket_ (glBindTexture tex) (glUnbindTexture tex)

--------------------------------------------------------------------------------
-- void SDL_DisableScreenSaver(void)
foreign import ccall unsafe "SDL_DisableScreenSaver"
  disableScreenSaver :: IO ()

-- void SDL_EnableScreenSaver(void)
foreign import ccall unsafe "SDL_EnableScreenSaver"
  enableScreenSaver :: IO ()

withoutScreenSaver :: IO a -> IO a
withoutScreenSaver = bracket_ disableScreenSaver enableScreenSaver

-- SDL_bool SDL_IsScreenSaverEnabled(void)
foreign import ccall unsafe "SDL_IsScreenSaverEnabled"
  sdlIsScreenSaverEnabled :: IO SDL_bool

isScreenSaverEnabled :: IO Bool
isScreenSaverEnabled = fmap (/= 0) sdlIsScreenSaverEnabled

-- void SDL_HideWindow(SDL_Window* window)
foreign import ccall unsafe "SDL_HideWindow" sdlHideWindow :: Ptr WindowStruct -> IO ()

hideWindow :: Window -> IO ()
hideWindow win = withForeignPtr win sdlHideWindow

-- void SDL_MaximizeWindow(SDL_Window* window)
foreign import ccall unsafe "SDL_MaximizeWindow" sdlMaximizeWindow :: Ptr WindowStruct -> IO ()

maximizeWindow :: Window -> IO ()
maximizeWindow win = withForeignPtr win sdlMaximizeWindow

-- void SDL_MinimizeWindow(SDL_Window* window)
foreign import ccall unsafe "SDL_MinimizeWindow" sdlMinimizeWindow :: Ptr WindowStruct -> IO ()

minimizeWindow :: Window -> IO ()
minimizeWindow win = withForeignPtr win sdlMinimizeWindow

-- void SDL_RaiseWindow(SDL_Window* window)
foreign import ccall unsafe "SDL_RaiseWindow" sdlRaiseWindow :: Ptr WindowStruct -> IO ()

raiseWindow :: Window -> IO ()
raiseWindow win = withForeignPtr win sdlRaiseWindow

-- void SDL_RestoreWindow(SDL_Window* window)
foreign import ccall unsafe "SDL_RestoreWindow" sdlRestoreWindow :: Ptr WindowStruct -> IO ()

restoreWindow :: Window -> IO ()
restoreWindow win = withForeignPtr win sdlRestoreWindow

-- void SDL_ShowWindow(SDL_Window* window)
foreign import ccall unsafe "SDL_ShowWindow" sdlShowWindow :: Ptr WindowStruct -> IO ()

showWindow :: Window -> IO ()
showWindow win = withForeignPtr win sdlShowWindow

-- int SDL_SetWindowBrightness(SDL_Window* window, float brightness)
foreign import ccall unsafe "SDL_SetWindowBrightness"
  sdlSetWindowBrightness :: Ptr WindowStruct -> CFloat -> IO CInt

setWindowBrightness :: Window -> Double -> IO ()
setWindowBrightness win brightness =
  unwrapBool "setWindowBrightness" $
  withForeignPtr win $ \cw ->
    fmap (==0) (sdlSetWindowBrightness cw (realToFrac brightness))

-- float SDL_GetWindowBrightness(SDL_Window* window)
foreign import ccall unsafe "SDL_GetWindowBrightness"
  sdlGetWindowBrightness :: Ptr WindowStruct -> IO CFloat

-- FIXME: Error handling?
getWindowBrightness :: Window -> IO Double
getWindowBrightness win =
  withForeignPtr win $
  fmap realToFrac . sdlGetWindowBrightness

-- void* SDL_SetWindowData(SDL_Window* window, const char* name, void* userdata)
-- void* SDL_GetWindowData(SDL_Window* window, const char* name)
-- int SDL_SetWindowDisplayMode(SDL_Window* window, const SDL_DisplayMode* mode)
-- int SDL_GetWindowDisplayMode(SDL_Window* window, SDL_DisplayMode* mode)
-- int SDL_SetWindowFullscreen(SDL_Window* window, Uint32 flags)
-- int SDL_SetWindowGammaRamp(SDL_Window*window,const Uint16* red,const Uint16* green,const Uint16* blue)
-- int SDL_GetWindowGammaRamp(SDL_Window* window,Uint16*red,Uint16*green,Uint16*blue)

-- void SDL_SetWindowGrab(SDL_Window* window, SDL_bool    grabbed)
foreign import ccall unsafe "SDL_SetWindowGrab"
  sdlSetWindowGrab :: Ptr WindowStruct -> SDL_bool -> IO ()

setWindowGrab :: Window -> Bool -> IO ()
setWindowGrab win flag =
  withForeignPtr win $ \cw ->
  sdlSetWindowGrab cw (if flag then 1 else 0)

-- SDL_bool SDL_GetWindowGrab(SDL_Window* window)
foreign import ccall unsafe "SDL_GetWindowGrab"
  sdlGetWindowGrab :: Ptr WindowStruct -> IO SDL_bool

getWindowGrab :: Window -> IO Bool
getWindowGrab win = withForeignPtr win $ fmap (/=0) . sdlGetWindowGrab

-- void SDL_SetWindowIcon(SDL_Window*  window, SDL_Surface* icon)

-- void SDL_SetWindowMaximumSize(SDL_Window* window,int max_w,int max_h)
foreign import ccall unsafe "SDL_SetWindowMaximumSize"
  sdlSetWindowMaximumSize :: Ptr WindowStruct -> CInt -> CInt -> IO ()

setWindowMaximumSize :: Window -> Size -> IO ()
setWindowMaximumSize win (Size width height) =
  withForeignPtr win $ \cw ->
  sdlSetWindowMaximumSize cw (fromIntegral height) (fromIntegral width)

-- void SDL_GetWindowMaximumSize(SDL_Window* window,int*w,int*h)
foreign import ccall unsafe "SDL_GetWindowMaximumSize"
  sdlGetWindowMaximumSize :: Ptr WindowStruct -> Ptr CInt -> Ptr CInt -> IO ()

getWindowMaximumSize :: Window -> IO Size
getWindowMaximumSize win =
  withForeignPtr win $ \cw ->
  alloca $ \widthPtr ->
  alloca $ \heightPtr -> do
    sdlGetWindowMaximumSize cw widthPtr heightPtr
    mkSize <$> peek widthPtr <*> peek heightPtr

-- void SDL_SetWindowMinimumSize(SDL_Window* window,int min_w,int min_h)
foreign import ccall unsafe "SDL_SetWindowMinimumSize"
  sdlSetWindowMinimumSize :: Ptr WindowStruct -> CInt -> CInt -> IO ()

setWindowMinimumSize :: Window -> Size -> IO ()
setWindowMinimumSize win (Size width height) =
  withForeignPtr win $ \cw ->
  sdlSetWindowMinimumSize cw (fromIntegral width) (fromIntegral height)

-- void SDL_GetWindowMinimumSize(SDL_Window* window, int*w, int*h)
foreign import ccall unsafe "SDL_GetWindowMinimumSize"
  sdlGetWindowMinimumSize :: Ptr WindowStruct -> Ptr CInt -> Ptr CInt -> IO ()

getWindowMinimumSize :: Window -> IO Size
getWindowMinimumSize win =
  withForeignPtr win $ \cw ->
  alloca $ \widthPtr ->
  alloca $ \heightPtr -> do
    sdlGetWindowMinimumSize cw widthPtr heightPtr
    mkSize <$> peek widthPtr <*> peek heightPtr

-- void SDL_SetWindowPosition(SDL_Window* window, int x, int y)
foreign import ccall unsafe "SDL_SetWindowPosition"
  sdlSetWindowPosition :: Ptr WindowStruct -> CInt -> CInt -> IO ()

setWindowPosition :: Window -> Position -> IO ()
setWindowPosition win (Position x y) =
  withForeignPtr win $ \cw ->
  sdlSetWindowPosition cw (fromIntegral x) (fromIntegral y)

-- void SDL_GetWindowPosition(SDL_Window* window, int*x, int*y)
foreign import ccall unsafe "SDL_GetWindowPosition"
  sdlGetWindowPosition :: Ptr WindowStruct -> Ptr CInt -> Ptr CInt -> IO ()

getWindowPosition :: Window -> IO Position
getWindowPosition win =
  withForeignPtr win $ \cw ->
  alloca $ \xPtr ->
  alloca $ \yPtr -> do
    sdlGetWindowPosition cw xPtr yPtr
    mkPosition <$> peek xPtr <*> peek yPtr

-- void SDL_SetWindowSize(SDL_Window* window, int w, int h)
foreign import ccall unsafe "SDL_SetWindowSize"
  sdlSetWindowSize :: Ptr WindowStruct -> CInt -> CInt -> IO ()

setWindowSize :: Window -> Size -> IO ()
setWindowSize win (Size width height) =
  withForeignPtr win $ \cw ->
  sdlSetWindowSize cw (fromIntegral width) (fromIntegral height)

-- void SDL_GetWindowSize(SDL_Window* window, int*w, int*h)
foreign import ccall unsafe "SDL_GetWindowSize"
  sdlGetWindowSize :: Ptr WindowStruct -> Ptr CInt -> Ptr CInt -> IO ()

getWindowSize :: Window -> IO Size
getWindowSize win =
  withForeignPtr win $ \cw ->
  alloca $ \widthPtr ->
  alloca $ \heightPtr -> do
    sdlGetWindowSize cw widthPtr heightPtr
    mkSize <$> peek widthPtr <*> peek heightPtr

-- void SDL_SetWindowTitle(SDL_Window* window, const char* title)
-- const char* SDL_GetWindowTitle(SDL_Window* window)




-------------------------------------------------------------------
-- Clipboard Handling

-- char* SDL_GetClipboardText(void)
foreign import ccall unsafe "SDL_GetClipboardText"
  sdlGetClipboardText :: IO CString

-- FIXME: Throw error if 'cstr' is NULL.
-- | Use this function to get UTF-8 text from the clipboard.
getClipboardText :: IO Text
getClipboardText = do
  cstr <- sdlGetClipboardText
  bs <- packCString cstr
  sdlFree cstr
  return $! decodeUtf8 bs

-- int SDL_SetClipboardText(const char* text)
foreign import ccall unsafe "SDL_SetClipboardText"
  sdlSetClipboardText :: CString -> IO CInt

-- | Use this function to put UTF-8 text into the clipboard.
setClipboardText :: Text -> IO ()
setClipboardText txt =
  useAsCString (encodeUtf8 txt) $ \cstr -> do
    unwrapBool "setClipboardText" (fmap (==0) (sdlSetClipboardText cstr))


-- SDL_bool SDL_HasClipboardText(void)
foreign import ccall unsafe "SDL_HasClipboardText"
  sdlHasClipboardText :: IO SDL_bool

-- | Use this function to return a flag indicating whether the clipboard
--   exists and contains a text string that is non-empty.
hasClipboardText :: IO Bool
hasClipboardText = fmap (/=0) sdlHasClipboardText

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_LoadBMP_RW"
  sdlLoadBMP :: Ptr RWopsStruct -> CInt -> IO (Ptr SurfaceStruct)

-- TODO Decide if this should be partial or return Maybe/Either
loadBMP :: FilePath -> IO Surface
loadBMP path =
  RWOps.withFile path "r" $ \rwops ->
  withForeignPtr rwops $ \crwops -> do
    bmp <- sdlLoadBMP crwops 0
    if bmp == nullPtr
      then error "loadBMP: failed to load BMP"
      else mkFinalizedSurface bmp

foreign import ccall unsafe "&SDL_FreeSurface"
  sdlFreeSurface_finalizer :: FunPtr (Ptr SurfaceStruct -> IO ())

freeSurface :: Surface -> IO ()
freeSurface = finalizeForeignPtr

foreign import ccall unsafe "SDL_SetColorKey"
  sdlSetColorKey :: Ptr SurfaceStruct -> CInt -> Word32 -> IO CInt

setColorKey :: Surface -> Bool -> Word32 -> IO ()
setColorKey s enabled pixel =
  unwrapBool "setColorKey" $
  withForeignPtr s $ \cs ->
  (== 0) <$> sdlSetColorKey cs (if enabled then 1 else 0) pixel

-------------------------------------------------------------------
-- Misc utilities

foreign import ccall unsafe "SDL_free"
  sdlFree :: Ptr a -> IO ()

mkFinalizedSurface :: Ptr SurfaceStruct -> IO Surface
mkFinalizedSurface = newForeignPtr sdlFreeSurface_finalizer

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_GetWindowPixelFormat"
  sdlGetWindowPixelFormat :: Ptr WindowStruct -> IO Word32

foreign import ccall unsafe "SDL_AllocFormat"
  sdlAllocFormat :: Word32 -> IO (Ptr PixelFormatStruct)

getWindowPixelFormat :: Window -> IO Word32
getWindowPixelFormat w = withForeignPtr w sdlGetWindowPixelFormat

allocFormat :: Word32 -> IO PixelFormat
allocFormat pf = sdlAllocFormat pf >>= newForeignPtr sdlFreeFormat_finalizer

foreign import ccall unsafe "&SDL_FreeFormat"
  sdlFreeFormat_finalizer :: FunPtr (Ptr PixelFormatStruct -> IO ())

foreign import ccall unsafe "SDL_MapRGB"
  sdlMapRGB :: Ptr PixelFormatStruct -> Word8 -> Word8 -> Word8 -> IO Word32

mapRGB :: PixelFormat -> Word8 -> Word8 -> Word8 -> IO Word32
mapRGB p r g b = withForeignPtr p $ \cp -> sdlMapRGB cp r g b

foreign import ccall unsafe "SDL_MapRGBA"
  sdlMapRGBA :: Ptr PixelFormatStruct -> Word8 -> Word8 -> Word8 -> Word8 -> IO Word32

mapRGBA :: PixelFormat -> Word8 -> Word8 -> Word8 -> Word8 -> IO Word32
mapRGBA p r g b a = withForeignPtr p $ \cp -> sdlMapRGBA cp r g b a

surfaceFormat :: Surface -> IO PixelFormat
surfaceFormat s =
  withForeignPtr s $ \cs ->
  #{peek SDL_Surface, format} cs >>= newForeignPtr_
