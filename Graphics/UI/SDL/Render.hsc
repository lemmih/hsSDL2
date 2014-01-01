#include "SDL.h"


module Graphics.UI.SDL.Render
  ( -- * http://wiki.libsdl.org/CategoryRender
    createRenderer
  , createSoftwareRenderer
  , createTexture
  , createTextureFromSurface
  , createWindowAndRenderer
  , destroyRenderer
  , destroyTexture
  , getNumRenderDrivers
  , getRenderDrawBlendMode
  , getRenderDrawColor
  , getRenderDriverInfo
  , getRenderTarget
  , getRenderer
  , getRendererInfo
  , getRendererOutputSize
  , getTextureAlphaMod
  , getTextureBlendMode
  , getTextureColorMod
  , lockTexture
  , queryTexture
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
  , renderGetLogicalSize
  , renderGetScale
  , renderGetViewport
  , renderPresent
  , renderReadPixels
  , renderSetClipRect
  , renderSetLogicalSize
  , renderSetScale
  , renderSetViewport
  , renderTargetSupported
  , setRenderDrawBlendMode
  , setRenderDrawColor
  , setRenderTarget
  , setTextureAlphaMod
  , setTextureBlendMode
  , setTextureColorMod
  , unlockTexture
  , updateTexture
  , updateYUVTexture

  -- * Data types
  , RenderingDevice(..)
  , RendererInfo(..)
  , RendererFlag(..)
  , BlendMode(..)
  , Flip(..)

  -- * extra stuff
  , withRenderer
  , withTexture
  ) where


import           Control.Applicative     ((<$>), (<*>), (<$>))
import           Data.Word
import Control.Exception (finally)
import Control.Monad (liftM)
import           Foreign.C
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Utils
import           Foreign.Ptr
import           Foreign.Storable
import qualified Data.Vector.Storable as V
import Control.Exception                     ( bracket, bracket_ )

import           Graphics.UI.SDL.General (unwrapBool)
import           Graphics.UI.SDL.Rect
import           Graphics.UI.SDL.Types
import Graphics.UI.SDL.Color
import Graphics.UI.SDL.Utilities (toBitmask)


data BlendMode = None | Blend | Add | Mod deriving (Eq, Show)

blendModeToCInt :: BlendMode -> CInt
blendModeToCInt None = #{ const SDL_BLENDMODE_NONE }
blendModeToCInt Blend = #{ const SDL_BLENDMODE_BLEND }
blendModeToCInt Add = #{ const SDL_BLENDMODE_ADD }
blendModeToCInt Mod = #{ const SDL_BLENDMODE_MOD }


data Flip = Horizontal | Vertical

flipToC :: Flip -> CInt
flipToC Horizontal = #{const SDL_FLIP_HORIZONTAL}
flipToC Vertical = #{const SDL_FLIP_VERTICAL}


data RendererInfo


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

foreign import ccall unsafe "SDL_CreateTexture"
  sdlCreateTexture :: Ptr RendererStruct -> Word32 -> CInt -> CInt -> CInt -> IO (Ptr TextureStruct)

foreign import ccall unsafe "&SDL_DestroyTexture"
  sdlDestroyTexture_finalizer :: FunPtr (Ptr TextureStruct -> IO ())

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


createWindowAndRenderer :: Int -> Int -> Word32 -> Window -> IO Renderer
createWindowAndRenderer = undefined


foreign import ccall unsafe "&SDL_DestroyRenderer"
  sdlDestroyRenderer_finalizer :: FunPtr (Ptr RendererStruct -> IO ())

destroyRenderer :: Renderer -> IO ()
destroyRenderer = finalizeForeignPtr

destroyTexture :: Texture -> IO ()
destroyTexture = finalizeForeignPtr

withTexture :: Renderer -> PixelFormatEnum -> TextureAccess -> Int -> Int -> (Texture -> IO r) -> IO r
withTexture r f t w h a = bracket (createTexture r f t w h) destroyTexture a


foreign import ccall unsafe "SDL_GetNumRenderDrivers"
  sdlGetNumRenderDrivers :: IO CInt

getNumRenderDrivers :: IO Int
getNumRenderDrivers = liftM fromIntegral sdlGetNumRenderDrivers


getRenderDrawBlendMode :: Renderer -> BlendMode -> IO ()
getRenderDrawBlendMode = undefined


getRenderDrawColor :: Renderer -> IO Color
getRenderDrawColor = undefined

getRenderDriverInfo :: Int -> IO RendererInfo
getRenderDriverInfo = undefined


getRenderTarget :: Renderer -> IO Texture
getRenderTarget = undefined

getRenderer :: Window -> IO Renderer
getRenderer = undefined

getRendererInfo :: Renderer -> IO RendererInfo
getRendererInfo = undefined

getRendererOutputSize :: Renderer -> IO (Int, Int)
getRendererOutputSize = undefined


getTextureAlphaMod :: Texture -> IO Int
getTextureAlphaMod = undefined

getTextureBlendMode :: Texture -> IO BlendMode
getTextureBlendMode = undefined

getTextureColorMod :: Texture -> IO (Int, Int, Int)
getTextureColorMod = undefined

foreign import ccall unsafe "SDL_LockTexture"
  sdlLockTexture :: Ptr TextureStruct -> Ptr Rect -> Ptr a -> Ptr CInt -> IO CInt

lockTexture :: Texture -> Maybe Rect -> ((Ptr a, Int) -> IO r) -> IO r
lockTexture texture rect f =
  withForeignPtr texture $ \ct ->
  maybeWith with rect $ \cr ->
  alloca $ \pixelPtr ->
  alloca $ \pitchPtr ->
  finally (do
      r <- sdlLockTexture ct cr pixelPtr pitchPtr

      pixels <- peek pixelPtr
      pitch <- peek pitchPtr

      f (pixels, fromIntegral pitch))
    (sdlUnlockTexture ct)


foreign import ccall unsafe "SDL_QueryTexture"
  sdlQueryTexture :: Ptr TextureStruct -> Ptr CUInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()

-- | Use this function to query the size of a texture.
queryTexture :: Texture -> IO Size
queryTexture texture =
  withForeignPtr texture $ \ct ->
  alloca $ \widthPtr ->
  alloca $ \heightPtr -> do
    sdlQueryTexture ct nullPtr nullPtr widthPtr heightPtr
    mkSize <$> peek widthPtr <*> peek heightPtr


foreign import ccall unsafe "SDL_RenderClear"
  sdlRenderClear :: Ptr RendererStruct -> IO Int

renderClear :: Renderer -> IO ()
renderClear renderer =
  unwrapBool "renderClear" $ withForeignPtr renderer $
    fmap (== 0) . sdlRenderClear


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


foreign import ccall unsafe "SDL_RenderGetClipRect"
  sdlRenderGetClipRect :: Ptr RendererStruct -> Ptr Rect -> IO ()

renderGetClipRect :: Renderer -> IO Rect
renderGetClipRect renderer =
  withForeignPtr renderer $ \r ->
  alloca $ \rect -> do
    sdlRenderGetClipRect r rect
    peek rect

renderGetLogicalSize :: Renderer -> IO (Int, Int)
renderGetLogicalSize = undefined

renderGetScale :: Renderer -> IO (Float, Float)
renderGetScale = undefined



foreign import ccall unsafe "SDL_RenderGetViewport"
  sdlRenderGetViewport :: Ptr RendererStruct -> Ptr Rect -> IO ()

renderGetViewport :: Renderer -> IO Rect
renderGetViewport renderer =
  withForeignPtr renderer $ \r ->
  alloca $ \rect -> do
    sdlRenderGetViewport r rect
    peek rect


foreign import ccall unsafe "SDL_RenderPresent"
  sdlRenderPresent :: Ptr RendererStruct -> IO ()

renderPresent :: Renderer -> IO ()
renderPresent renderer = withForeignPtr renderer $ sdlRenderPresent


renderReadPixels = undefined


foreign import ccall unsafe "SDL_RenderSetClipRect"
  sdlRenderSetClipRect :: Ptr RendererStruct -> Ptr Rect -> IO CInt

renderSetClipRect :: Renderer -> Rect -> IO ()
renderSetClipRect renderer rect =
  unwrapBool "renderSetClipRect" $
  withForeignPtr renderer $ \r ->
  with rect $ \cr ->
  (== 0) <$> sdlRenderSetClipRect r cr


foreign import ccall unsafe "SDL_RenderSetLogicalSize"
    sdlRenderSetLogicalSize :: Ptr RendererStruct -> CInt -> CInt -> IO ()

renderSetLogicalSize :: Renderer -> Int -> Int -> IO ()
renderSetLogicalSize renderer width height = withForeignPtr renderer $ \r ->
    sdlRenderSetLogicalSize r (fromIntegral width) (fromIntegral height)

renderSetScale :: Renderer -> Float -> Float -> IO ()
renderSetScale = undefined

foreign import ccall unsafe "SDL_RenderSetViewport"
  sdlRenderSetViewport :: Ptr RendererStruct -> Ptr Rect -> IO CInt

renderSetViewport :: Renderer -> Rect -> IO ()
renderSetViewport renderer rect =
  unwrapBool "renderSetViewport" $
  withForeignPtr renderer $ \r ->
  with rect $ \cr ->
  (== 0) <$> sdlRenderSetViewport r cr


renderTargetSupported :: Renderer -> IO Bool
renderTargetSupported = undefined


foreign import ccall unsafe "SDL_SetRenderDrawBlendMode"
  sdlSetRenderDrawBlendMode :: Ptr RendererStruct -> CInt -> IO CInt

setRenderDrawBlendMode :: Renderer -> BlendMode -> IO ()
setRenderDrawBlendMode renderer blendMode =
  unwrapBool "setRenderDrawBlendMode" $
  withForeignPtr renderer $ \r ->
  (== 0) <$> sdlSetRenderDrawBlendMode r (blendModeToCInt blendMode)


foreign import ccall unsafe "SDL_SetRenderDrawColor"
  sdlSetRenderDrawColor :: Ptr RendererStruct -> Word8 -> Word8 -> Word8 -> Word8 -> IO Int

setRenderDrawColor :: Renderer -> Word8 -> Word8 -> Word8 -> Word8 -> IO ()
setRenderDrawColor renderer r g b a =
  unwrapBool "setRenderDrawColor" $ withForeignPtr renderer $ \cR ->
    (== 0) <$> sdlSetRenderDrawColor cR r g b a

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


foreign import ccall unsafe "SDL_SetTextureBlendMode"
  sdlSetTextureBlendMode :: Ptr TextureStruct -> CInt -> IO CInt

setTextureBlendMode :: Texture -> BlendMode -> IO ()
setTextureBlendMode t b =
  unwrapBool "setTextureBlendMode" $
  withForeignPtr t $ \ct ->
  (== 0) <$> sdlSetTextureBlendMode ct (blendModeToCInt b)


foreign import ccall unsafe "SDL_SetTextureColorMod"
  sdlSetTextureColorMod :: Ptr TextureStruct -> Word8 -> Word8 -> Word8 -> IO CInt

setTextureColorMod :: Texture -> Word8 -> Word8 -> Word8 -> IO ()
setTextureColorMod texture r g b =
  unwrapBool "setTextureColorMod" $
  withForeignPtr texture $ \t ->
  (== 0) <$> sdlSetTextureColorMod t r g b


foreign import ccall unsafe "SDL_UnlockTexture"
  sdlUnlockTexture :: Ptr TextureStruct -> IO ()

unlockTexture :: Texture -> IO ()
unlockTexture = undefined


foreign import ccall unsafe "SDL_UpdateTexture"
  sdlUpdateTexture :: Ptr TextureStruct -> Ptr Rect -> Ptr a -> CInt -> IO CInt

updateTexture :: Texture -> Rect -> Ptr a -> Int -> IO ()
updateTexture texture rect pixels pitch =
  unwrapBool "updateTexture" $
  withForeignPtr texture $ \t ->
  with rect $ \r ->
  (== 0) <$> sdlUpdateTexture t r pixels (fromIntegral pitch)

updateYUVTexture = undefined
