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
  ( createWindow
  , withWindow
  , destroyWindow
  , createRenderer
  , destroyRenderer
  , setRenderDrawColor
  , renderClear
  , renderPresent
  , disableScreenSaver
  , enableScreenSaver
  , withoutScreenSaver
  , isScreenSaverEnabled
    -- * Clipboard handling
  , getClipboardText
  , setClipboardText
  , hasClipboardText
  ) where

import Control.Applicative
import Foreign.C.Types
import Foreign.C
import Foreign
import Control.Exception  (bracket, bracket_)
import Data.Text.Encoding
import qualified Data.Text as T
import Data.Text ( Text )
import Data.ByteString

import Graphics.UI.SDL.Types
import Graphics.UI.SDL.Utilities (toBitmask)
import Graphics.UI.SDL.General

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

createWindow :: String -> Int -> Int -> Int -> Int -> IO Window
createWindow title x y w h =
  withUtf8CString title $ \cstr -> do
    window <- sdlCreateWindow cstr (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h) 0
    newForeignPtr sdlDestroyWindow_finalizer window

withWindow :: String -> Int -> Int -> Int -> Int -> (Window -> IO r) -> IO r
withWindow title x y w h action =
  bracket (createWindow title x y w h) destroyWindow action

data RenderingDevice = Device Int | FirstSupported

data RendererFlag = Software | Accelerated | PresentVSync | TargetTexture

instance Enum RendererFlag where
  fromEnum Software = #{const SDL_RENDERER_SOFTWARE}
  fromEnum Accelerated = #{const SDL_RENDERER_ACCELERATED}
  fromEnum PresentVSync = #{const SDL_RENDERER_PRESENTVSYNC}
  fromEnum TargetTexture = #{const SDL_RENDERER_TARGETTEXTURE}

  toEnum #{const SDL_RENDERER_SOFTWARE} = Software
  toEnum #{const SDL_RENDERER_ACCELERATED} = Accelerated
  toEnum #{const SDL_RENDERER_PRESENTVSYNC} = PresentVSync
  toEnum #{const SDL_RENDERER_TARGETTEXTURE} = TargetTexture
  toEnum _ = error "Graphics.UI.SDL.Video.toEnum (RendererFlag): bad argument"

  succ Software = Accelerated
  succ Accelerated = PresentVSync
  succ PresentVSync = TargetTexture
  succ _ = error "Graphics.UI.SDL.Video.succ (RendererFlag): bad argument"

  pred Accelerated = Software
  pred PresentVSync = Accelerated
  pred TargetTexture = PresentVSync
  pred _ = error "Graphics.UI.SDL.Video.pred (RendererFlag): bad argument"

foreign import ccall unsafe "SDL_CreateRenderer"
  sdlCreateRenderer :: Ptr WindowStruct -> CInt -> CUInt -> IO (Ptr RendererStruct)

createRenderer :: Window -> RenderingDevice -> [RendererFlag] -> IO Renderer
createRenderer w d flags = withForeignPtr w $ \cW -> do
  renderer <- sdlCreateRenderer cW device (toBitmask flags)
  if renderer == nullPtr
    then error "createRenderer: Failed to create rendering context"
    else newForeignPtr sdlDestroyRenderer_finalizer renderer
  where device = case d of
                   Device n -> fromIntegral n
                   FirstSupported -> 0

withRenderer :: Window -> RenderingDevice -> [RendererFlag] -> (Renderer -> IO r) -> IO r
withRenderer w d f a = bracket (createRenderer w d f) destroyRenderer a

foreign import ccall unsafe "&SDL_DestroyRenderer"
  sdlDestroyRenderer_finalizer :: FunPtr (Ptr RendererStruct -> IO ())

destroyRenderer :: Renderer -> IO ()
destroyRenderer = finalizeForeignPtr

foreign import ccall unsafe "SDL_SetRenderDrawColor"
  sdlSetRenderDrawColor :: Ptr RendererStruct -> Word8 -> Word8 -> Word8 -> Word8 -> IO Int

setRenderDrawColor :: Renderer -> Word8 -> Word8 -> Word8 -> Word8 -> IO Bool
setRenderDrawColor renderer r g b a = withForeignPtr renderer $ \cR ->
  (== 0) <$> sdlSetRenderDrawColor cR r g b a

foreign import ccall unsafe "SDL_RenderClear"
  sdlRenderClear :: Ptr RendererStruct -> IO Int

renderClear :: Renderer -> IO Bool
renderClear renderer = withForeignPtr renderer $
  fmap (== 0) . sdlRenderClear

foreign import ccall unsafe "SDL_RenderPresent"
  sdlRenderPresent :: Ptr RendererStruct -> IO Int

renderPresent :: Renderer -> IO Bool
renderPresent renderer = withForeignPtr renderer $
  fmap (== 0) . sdlRenderPresent

-- void SDL_DestroyWindow(SDL_Window* window)

foreign import ccall unsafe "&SDL_DestroyWindow"
  sdlDestroyWindow_finalizer :: FunPtr (Ptr WindowStruct -> IO ())

destroyWindow :: Window -> IO ()
destroyWindow = finalizeForeignPtr

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
-- void SDL_MaximizeWindow(SDL_Window* window)
-- void SDL_MinimizeWindow(SDL_Window* window)
-- void SDL_RaiseWindow(SDL_Window* window)
-- void SDL_RestoreWindow(SDL_Window* window)
-- void SDL_ShowWindow(SDL_Window* window)

-- int SDL_SetWindowBrightness(SDL_Window* window, float brightness)
-- float SDL_GetWindowBrightness(SDL_Window* window)
-- void* SDL_SetWindowData(SDL_Window* window, const char* name, void* userdata)
-- void* SDL_GetWindowData(SDL_Window* window, const char* name)
-- int SDL_SetWindowDisplayMode(SDL_Window* window, const SDL_DisplayMode* mode)
-- int SDL_GetWindowDisplayMode(SDL_Window* window, SDL_DisplayMode* mode)
-- int SDL_SetWindowFullscreen(SDL_Window* window, Uint32 flags)
-- int SDL_SetWindowGammaRamp(SDL_Window*window,const Uint16* red,const Uint16* green,const Uint16* blue)
-- int SDL_GetWindowGammaRamp(SDL_Window* window,Uint16*red,Uint16*green,Uint16*blue)
-- void SDL_SetWindowGrab(SDL_Window* window, SDL_bool    grabbed)
-- SDL_bool SDL_GetWindowGrab(SDL_Window* window)
-- void SDL_SetWindowIcon(SDL_Window*  window, SDL_Surface* icon)
-- void SDL_SetWindowMaximumSize(SDL_Window* window,int max_w,int max_h)
-- void SDL_GetWindowMaximumSize(SDL_Window* window,int*w,int*h)
-- void SDL_SetWindowMinimumSize(SDL_Window* window,int min_w,int min_h)
-- void SDL_GetWindowMinimumSize(SDL_Window* window, int*w, int*h)
-- void SDL_SetWindowPosition(SDL_Window* window, int x, int y)
-- void SDL_GetWindowPosition(SDL_Window* window, int*x, int*y)
-- void SDL_SetWindowSize(SDL_Window* window, int w, int h)
-- void SDL_GetWindowSize(SDL_Window* window, int*w, int*h)
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



-------------------------------------------------------------------
-- Misc utilities

foreign import ccall unsafe "SDL_free"
  sdlFree :: Ptr a -> IO ()

