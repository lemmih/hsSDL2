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

    -- * Renderers
    -- ** Creation/Destruction
  , withRenderer
  , RenderingDevice(..)
  , RendererFlag(..)
  , createRenderer
  , createSoftwareRenderer
  , destroyRenderer

    -- ** Rendering
  , renderPresent
  , renderClear
  , renderCopy
  , renderCopyEx
  , renderDrawLine
  , renderDrawLines
  , renderDrawPoint
  , renderDrawPoints
  , renderDrawRect
  , renderDrawRects
  , renderFillRect
  , renderFillRects
  , renderGetClipRect
  , renderGetViewport
  , renderSetClipRect
  , renderSetViewport
  , renderSetLogicalSize
  , setRenderDrawBlendMode
  , setRenderDrawColor
  , BlendMode(..)
  , Flip(..)

    -- ** Textures
  , createTexture
  , createTextureFromSurface
  , queryTextureSize
  , setTextureBlendMode
  , setTextureAlphaMod
  , setTextureColorMod
  , updateTexture

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
import Control.Monad
import Foreign.C.Types
import Foreign.C
import Foreign
import Control.Exception                     ( bracket, bracket_ )
import Data.Text.Encoding
import qualified Data.Text as T
import Data.Text                             ( Text )
import Data.ByteString                       ( useAsCString, packCString )
import qualified Data.Vector.Storable as V

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

foreign import ccall unsafe "SDL_CreateRenderer"
  sdlCreateRenderer :: Ptr WindowStruct -> CInt -> CUInt -> IO (Ptr RendererStruct)

createRenderer :: Window -> RenderingDevice -> [RendererFlag] -> IO Renderer
createRenderer w d flags = withForeignPtr w $ \cW -> do
  renderer <- sdlCreateRenderer cW device (toBitmask rendererFlagToC flags)
  if renderer == nullPtr
    then error "createRenderer: Failed to create rendering context"
    else newForeignPtr sdlDestroyRenderer_finalizer renderer
  where device = case d of
                   Device n -> fromIntegral n
                   FirstSupported -> 0

foreign import ccall unsafe "SDL_CreateSoftwareRenderer"
  sdlCreateSoftwareRenderer :: Ptr SurfaceStruct -> IO (Ptr RendererStruct)

createSoftwareRenderer :: Surface -> IO Renderer
createSoftwareRenderer s = withForeignPtr s $ \cS -> do
  renderer <- sdlCreateSoftwareRenderer cS
  if renderer == nullPtr
    then error "createSoftwareRenderer: Failed to create rendering context"
    else newForeignPtr sdlDestroyRenderer_finalizer renderer

withRenderer :: Window -> RenderingDevice -> [RendererFlag] -> (Renderer -> IO r) -> IO r
withRenderer w d f a = bracket (createRenderer w d f) destroyRenderer a

foreign import ccall unsafe "&SDL_DestroyRenderer"
  sdlDestroyRenderer_finalizer :: FunPtr (Ptr RendererStruct -> IO ())

destroyRenderer :: Renderer -> IO ()
destroyRenderer = finalizeForeignPtr

foreign import ccall unsafe "SDL_SetRenderDrawColor"
  sdlSetRenderDrawColor :: Ptr RendererStruct -> Word8 -> Word8 -> Word8 -> Word8 -> IO Int

setRenderDrawColor :: Renderer -> Word8 -> Word8 -> Word8 -> Word8 -> IO ()
setRenderDrawColor renderer r g b a =
  unwrapBool "setRenderDrawColor" $ withForeignPtr renderer $ \cR ->
    (== 0) <$> sdlSetRenderDrawColor cR r g b a

foreign import ccall unsafe "SDL_SetRenderDrawBlendMode"
  sdlSetRenderDrawBlendMode :: Ptr RendererStruct -> CInt -> IO CInt

setRenderDrawBlendMode :: Renderer -> BlendMode -> IO ()
setRenderDrawBlendMode renderer blendMode =
  unwrapBool "setRenderDrawBlendMode" $
  withForeignPtr renderer $ \r ->
  (== 0) <$> sdlSetRenderDrawBlendMode r (blendModeToCInt blendMode)

foreign import ccall unsafe "SDL_SetRenderTarget"
  sdlSetRenderTarget :: Ptr RendererStruct -> Ptr TextureStruct -> IO CInt

setRenderTarget :: Renderer -> Texture -> IO ()
setRenderTarget renderer texture =
  unwrapBool "setRenderTarget" $
  withForeignPtr renderer $ \r ->
  withForeignPtr texture $ \t ->
  (== 0) <$> sdlSetRenderTarget r t

foreign import ccall unsafe "SDL_SetTextureAlphaMod"
  sdlSetTextureAlphaMod :: Ptr TextureStruct -> Word8 -> IO CInt

setTextureAlphaMod :: Texture -> Word8 -> IO ()
setTextureAlphaMod texture alpha =
  unwrapBool "setTextureAlphaMod" $
  withForeignPtr texture $ \t ->
  (== 0) <$> sdlSetTextureAlphaMod t alpha

foreign import ccall unsafe "SDL_SetTextureColorMod"
  sdlSetTextureColorMod :: Ptr TextureStruct -> Word8 -> Word8 -> Word8 -> IO CInt

setTextureColorMod :: Texture -> Word8 -> Word8 -> Word8 -> IO ()
setTextureColorMod texture r g b =
  unwrapBool "setTextureColorMod" $
  withForeignPtr texture $ \t ->
  (== 0) <$> sdlSetTextureColorMod t r g b

foreign import ccall unsafe "SDL_UpdateTexture"
  sdlUpdateTexture :: Ptr TextureStruct -> Ptr Rect -> Ptr a -> CInt -> IO CInt

updateTexture :: Texture -> Rect -> Ptr a -> Int -> IO ()
updateTexture texture rect pixels pitch =
  unwrapBool "updateTexture" $
  withForeignPtr texture $ \t ->
  with rect $ \r ->
  (== 0) <$> sdlUpdateTexture t r pixels (fromIntegral pitch)

data BlendMode = None | Blend | Add | Mod deriving (Eq, Show)

blendModeToCInt :: BlendMode -> CInt
blendModeToCInt None = #{ const SDL_BLENDMODE_NONE }
blendModeToCInt Blend = #{ const SDL_BLENDMODE_BLEND }
blendModeToCInt Add = #{ const SDL_BLENDMODE_ADD }
blendModeToCInt Mod = #{ const SDL_BLENDMODE_MOD }

foreign import ccall unsafe "SDL_RenderClear"
  sdlRenderClear :: Ptr RendererStruct -> IO Int

renderClear :: Renderer -> IO ()
renderClear renderer =
  unwrapBool "renderClear" $ withForeignPtr renderer $
    fmap (== 0) . sdlRenderClear

foreign import ccall unsafe "SDL_RenderPresent"
  sdlRenderPresent :: Ptr RendererStruct -> IO ()

renderPresent :: Renderer -> IO ()
renderPresent renderer = withForeignPtr renderer $ sdlRenderPresent

foreign import ccall unsafe "SDL_RenderDrawLine"
  sdlRenderDrawLine :: Ptr RendererStruct -> CInt -> CInt -> CInt -> CInt -> IO CInt

renderDrawLine :: Renderer -> Word32 -> Word32 -> Word32 -> Word32 -> IO ()
renderDrawLine renderer x y x' y' =
  unwrapBool "renderDrawLine" $ withForeignPtr renderer $ \r ->
    (== 0) <$> sdlRenderDrawLine r (fromIntegral x) (fromIntegral y)
                                   (fromIntegral x') (fromIntegral y')

foreign import ccall unsafe "SDL_RenderDrawLines"
  sdlRenderDrawLines :: Ptr RendererStruct -> Ptr Point -> CInt -> IO CInt

renderDrawLines :: Renderer -> V.Vector Point -> IO ()
renderDrawLines renderer points =
  let (cfp, count) = V.unsafeToForeignPtr0 points
  in unwrapBool "renderDrawLines" $
     withForeignPtr renderer $ \r ->
     withForeignPtr cfp $ \cp ->
     (== 0) <$> sdlRenderDrawLines r cp (fromIntegral count)

foreign import ccall unsafe "SDL_RenderDrawPoint"
  sdlRenderDrawPoint :: Ptr RendererStruct -> CInt -> CInt -> IO CInt

renderDrawPoint :: Renderer -> Word32 -> Word32 -> IO ()
renderDrawPoint renderer x y =
  unwrapBool "renderDrawPoint" $ withForeignPtr renderer $ \r ->
    (== 0) <$> sdlRenderDrawPoint r (fromIntegral x) (fromIntegral y)

foreign import ccall unsafe "SDL_RenderDrawPoints"
  sdlRenderDrawPoints :: Ptr RendererStruct -> Ptr Point -> CInt -> IO CInt

renderDrawPoints :: Renderer -> V.Vector Point -> IO ()
renderDrawPoints renderer points =
  let (cfp, count) = V.unsafeToForeignPtr0 points
  in unwrapBool "renderDrawPoints" $
     withForeignPtr renderer $ \r ->
     withForeignPtr cfp $ \cp ->
     (== 0) <$> sdlRenderDrawPoints r cp (fromIntegral count)

foreign import ccall unsafe "SDL_RenderDrawRect"
  sdlRenderDrawRect :: Ptr RendererStruct -> Ptr Rect -> IO CInt

renderDrawRect :: Renderer -> Rect -> IO ()
renderDrawRect renderer rect =
  unwrapBool "renderDrawRect" $
  withForeignPtr renderer $ \r ->
  with rect $ \cr ->
  (== 0) <$> sdlRenderDrawRect r cr

foreign import ccall unsafe "SDL_RenderDrawRects"
  sdlRenderDrawRects :: Ptr RendererStruct -> Ptr Rect -> CInt -> IO CInt

renderDrawRects :: Renderer -> V.Vector Rect -> IO ()
renderDrawRects renderer rects =
  let (cfr, count) = V.unsafeToForeignPtr0 rects
  in unwrapBool "renderDrawRects" $
     withForeignPtr renderer $ \r ->
     withForeignPtr cfr $ \cr ->
     (== 0) <$> sdlRenderDrawRects r cr (fromIntegral count)

foreign import ccall unsafe "SDL_RenderFillRect"
  sdlRenderFillRect :: Ptr RendererStruct -> Ptr Rect -> IO CInt

renderFillRect :: Renderer -> Rect -> IO ()
renderFillRect renderer rect =
  unwrapBool "renderFillRect" $
  withForeignPtr renderer $ \r ->
  with rect $ \cr ->
  (== 0) <$> sdlRenderFillRect r cr

foreign import ccall unsafe "SDL_RenderFillRects"
  sdlRenderFillRects :: Ptr RendererStruct -> Ptr Rect -> CInt -> IO CInt

renderFillRects :: Renderer -> V.Vector Rect -> IO ()
renderFillRects renderer rects =
  let (cfr, count) = V.unsafeToForeignPtr0 rects
  in unwrapBool "renderFillRects" $
     withForeignPtr renderer $ \r ->
     withForeignPtr cfr $ \cr ->
     (== 0) <$> sdlRenderFillRects r cr (fromIntegral count)

foreign import ccall unsafe "SDL_RenderCopy"
  sdlRenderCopy :: Ptr RendererStruct -> Ptr TextureStruct -> Ptr Rect -> Ptr Rect -> IO CInt

renderCopy :: Renderer -> Texture -> Maybe Rect -> Maybe Rect -> IO ()
renderCopy renderer texture src dest =
  unwrapBool "renderCopy" $
  withForeignPtr renderer $ \cr ->
  withForeignPtr texture $ \ct ->
  maybeWith with src $ \csrc ->
  maybeWith with dest $ \cdest ->
  (== 0) <$> sdlRenderCopy cr ct csrc cdest

foreign import ccall unsafe "SDL_RenderCopyEx" sdlRenderCopyEx
  :: Ptr RendererStruct -> Ptr TextureStruct
  -> Ptr Rect -> Ptr Rect
  -> CDouble -> Ptr Point -> CInt
  -> IO CInt

data Flip = Horizontal | Vertical

flipToC :: Flip -> CInt
flipToC Horizontal = #{const SDL_FLIP_HORIZONTAL}
flipToC Vertical = #{const SDL_FLIP_VERTICAL}

renderCopyEx :: Renderer -> Texture -> Maybe Rect -> Maybe Rect -> Double -> Maybe Point -> [Flip] -> IO ()
renderCopyEx renderer texture src dest rotation origin flips =
  unwrapBool "renderCopyEx" $
  withForeignPtr renderer $ \cr ->
  withForeignPtr texture $ \ct ->
  maybeWith with src $ \csrc ->
  maybeWith with dest $ \cdest ->
  maybeWith with origin $ \corigin ->
  (== 0) <$> sdlRenderCopyEx cr ct csrc cdest (realToFrac rotation) corigin
               (toBitmask flipToC flips)

foreign import ccall unsafe "SDL_RenderGetClipRect"
  sdlRenderGetClipRect :: Ptr RendererStruct -> Ptr Rect -> IO ()

renderGetClipRect :: Renderer -> IO Rect
renderGetClipRect renderer =
  withForeignPtr renderer $ \r ->
  alloca $ \rect -> do
    sdlRenderGetClipRect r rect
    peek rect

foreign import ccall unsafe "SDL_RenderGetViewport"
  sdlRenderGetViewport :: Ptr RendererStruct -> Ptr Rect -> IO ()

renderGetViewport :: Renderer -> IO Rect
renderGetViewport renderer =
  withForeignPtr renderer $ \r ->
  alloca $ \rect -> do
    sdlRenderGetViewport r rect
    peek rect

foreign import ccall unsafe "SDL_RenderSetClipRect"
  sdlRenderSetClipRect :: Ptr RendererStruct -> Ptr Rect -> IO CInt

renderSetClipRect :: Renderer -> Rect -> IO ()
renderSetClipRect renderer rect =
  unwrapBool "renderSetClipRect" $
  withForeignPtr renderer $ \r ->
  with rect $ \cr ->
  (== 0) <$> sdlRenderSetClipRect r cr

foreign import ccall unsafe "SDL_RenderSetViewport"
  sdlRenderSetViewport :: Ptr RendererStruct -> Ptr Rect -> IO CInt

renderSetViewport :: Renderer -> Rect -> IO ()
renderSetViewport renderer rect =
  unwrapBool "renderSetViewport" $
  withForeignPtr renderer $ \r ->
  with rect $ \cr ->
  (== 0) <$> sdlRenderSetViewport r cr

foreign import ccall unsafe "SDL_CreateTexture"
  sdlCreateTexture :: Ptr RendererStruct -> Word32 -> CInt -> CInt -> CInt -> IO (Ptr TextureStruct)

createTexture :: Renderer -> PixelFormatEnum -> TextureAccess -> Int -> Int -> IO Texture
createTexture renderer format access w h =
  withForeignPtr renderer $ \cr -> do
    t <- sdlCreateTexture cr (pixelFormatEnumToC format) (textureAccessToC access) (fromIntegral w) (fromIntegral h)
    if t == nullPtr
      then error "createTexture"
      else newForeignPtr sdlDestroyTexture_finalizer t

foreign import ccall unsafe "SDL_CreateTextureFromSurface"
  sdlCreateTextureFromSurface :: Ptr RendererStruct -> Ptr SurfaceStruct -> IO (Ptr TextureStruct)

createTextureFromSurface :: Renderer -> Surface -> IO Texture
createTextureFromSurface renderer surface =
  withForeignPtr renderer $ \cr ->
  withForeignPtr surface $ \cs -> do
    t <- sdlCreateTextureFromSurface cr cs
    if t == nullPtr
      then error "createTextureFromSurface"
      else newForeignPtr sdlDestroyTexture_finalizer t

foreign import ccall unsafe "&SDL_DestroyTexture"
  sdlDestroyTexture_finalizer :: FunPtr (Ptr TextureStruct -> IO ())

-- int SDL_QueryTexture(SDL_Texture* texture,
--                      Uint32*      format,
--                      int*         access,
--                      int*         w,
--                      int*         h)
foreign import ccall unsafe "SDL_QueryTexture"
  sdlQueryTexture :: Ptr TextureStruct -> Ptr CUInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()

-- | Use this function to query the size of a texture.
queryTextureSize :: Texture -> IO Size
queryTextureSize texture =
  withForeignPtr texture $ \ct ->
  alloca $ \widthPtr ->
  alloca $ \heightPtr -> do
    sdlQueryTexture ct nullPtr nullPtr widthPtr heightPtr
    mkSize <$> peek widthPtr <*> peek heightPtr

foreign import ccall unsafe "SDL_SetTextureBlendMode"
  sdlSetTextureBlendMode :: Ptr TextureStruct -> CInt -> IO CInt

setTextureBlendMode :: Texture -> BlendMode -> IO ()
setTextureBlendMode t b =
  unwrapBool "setTextureBlendMode" $
  withForeignPtr t $ \ct ->
  (== 0) <$> sdlSetTextureBlendMode ct (blendModeToCInt b)
  
foreign import ccall unsafe "SDL_RenderSetLogicalSize"
    sdlRenderSetLogicalSize :: Ptr RendererStruct -> CInt -> CInt -> IO ()

renderSetLogicalSize :: Renderer -> Int -> Int -> IO ()
renderSetLogicalSize renderer width height = withForeignPtr renderer $ \r -> 
    sdlRenderSetLogicalSize r (fromIntegral width) (fromIntegral height) 

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
