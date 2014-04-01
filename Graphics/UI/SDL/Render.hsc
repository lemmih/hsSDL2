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
  --, getRenderDriverInfo
  , getRenderTarget
  , getRenderer
  --, getRendererInfo
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
  --, renderReadPixels
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
  --, updateYUVTexture

  -- * Data types
  , RenderingDevice(..)
  --, RendererInfo(..)
  , RendererFlag(..)
  , BlendMode(..)
  , Flip(..)

  -- * extra stuff
  , withRenderer
  , withTexture
  ) where


import           Control.Applicative       ((<$>), (<$>), (<*>))
import           Control.Exception         (finally)
import           Control.Exception         (bracket, bracket_)
import           Control.Monad             (liftM)
import qualified Data.Vector.Storable      as V
import Data.Int
import           Data.Word
import           Foreign.C
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Utils
import           Foreign.Ptr
import           Foreign.Storable

import           Graphics.UI.SDL.Error     (getError)
import           Graphics.UI.SDL.Color
import           Graphics.UI.SDL.General   (handleError, handleErrorI,
                                            unwrapBool)
import           Graphics.UI.SDL.Rect
import           Graphics.UI.SDL.Types
import           Graphics.UI.SDL.Utilities (toBitmask)
import           Graphics.UI.SDL.Video     (sdlDestroyWindow_finalizer)

data BlendMode = BMNone | BMBlend | BMAdd | BMMod deriving (Eq, Show)

blendModeToCInt :: BlendMode -> CInt
blendModeToCInt BMNone = #{ const SDL_BLENDMODE_NONE }
blendModeToCInt BMBlend = #{ const SDL_BLENDMODE_BLEND }
blendModeToCInt BMAdd = #{ const SDL_BLENDMODE_ADD }
blendModeToCInt BMMod = #{ const SDL_BLENDMODE_MOD }

cIntToBlendMode :: CInt -> BlendMode
cIntToBlendMode #{ const SDL_BLENDMODE_NONE } = BMNone
cIntToBlendMode #{ const SDL_BLENDMODE_BLEND } = BMBlend
cIntToBlendMode #{ const SDL_BLENDMODE_ADD } = BMAdd
cIntToBlendMode #{ const SDL_BLENDMODE_MOD } = BMMod


data Flip = Horizontal | Vertical deriving (Show)

flipToC :: Flip -> CInt
flipToC Horizontal = #{const SDL_FLIP_HORIZONTAL}
flipToC Vertical = #{const SDL_FLIP_VERTICAL}


foreign import ccall unsafe "SDL_CreateRenderer"
  sdlCreateRenderer :: Ptr WindowStruct -> CInt -> CUInt -> IO (Ptr RendererStruct)

createRenderer :: Window -> RenderingDevice -> [RendererFlag] -> IO Renderer
createRenderer w d flags = withForeignPtr w $ \cW -> do
    renderer <- sdlCreateRenderer cW device (toBitmask rendererFlagToC flags)
    handleError "createRenderer" renderer (newForeignPtr sdlDestroyRenderer_finalizer)
  where
    device = case d of
               Device n -> fromIntegral n
               FirstSupported -> 0


foreign import ccall unsafe "SDL_CreateSoftwareRenderer"
  sdlCreateSoftwareRenderer :: Ptr SurfaceStruct -> IO (Ptr RendererStruct)

createSoftwareRenderer :: Surface -> IO Renderer
createSoftwareRenderer s = withForeignPtr s $ \cS -> do
  renderer <- sdlCreateSoftwareRenderer cS
  handleError "createSoftwareRenderer" renderer (newForeignPtr sdlDestroyRenderer_finalizer)


withRenderer :: Window -> RenderingDevice -> [RendererFlag] -> (Renderer -> IO r) -> IO r
withRenderer w d f a = bracket (createRenderer w d f) destroyRenderer a


foreign import ccall unsafe "SDL_CreateTexture"
  sdlCreateTexture :: Ptr RendererStruct -> Word32 -> CInt -> CInt -> CInt -> IO (Ptr TextureStruct)

foreign import ccall unsafe "&SDL_DestroyTexture"
  sdlDestroyTexture_finalizer :: FunPtr (Ptr TextureStruct -> IO ())

createTexture :: Renderer -> PixelFormatEnum -> TextureAccess -> Int -> Int -> IO Texture
createTexture renderer format access w h =
  withForeignPtr renderer $ \cr -> do
    t <- sdlCreateTexture cr
                          (pixelFormatEnumToC format)
                          (textureAccessToC access)
                          (fromIntegral w)
                          (fromIntegral h)
    handleError "createTexture" t (newForeignPtr sdlDestroyTexture_finalizer)


foreign import ccall unsafe "SDL_CreateTextureFromSurface"
  sdlCreateTextureFromSurface :: Ptr RendererStruct -> Ptr SurfaceStruct -> IO (Ptr TextureStruct)

createTextureFromSurface :: Renderer -> Surface -> IO Texture
createTextureFromSurface renderer surface =
  withForeignPtr renderer $ \cr ->
  withForeignPtr surface $ \cs -> do
    t <- sdlCreateTextureFromSurface cr cs
    handleError "createTextureFromSurface" t (newForeignPtr sdlDestroyTexture_finalizer)


foreign import ccall unsafe "SDL_CreateWindowAndRenderer"
  sdlCreateWindowAndRenderer
    :: #{type int} -> #{type int} -> #{type Uint32}
    -> Ptr (Ptr WindowStruct) -> Ptr (Ptr RendererStruct) -> IO ( #{type int} )

createWindowAndRenderer :: Size -> [WindowFlag] -> IO (Window, Renderer)
createWindowAndRenderer (Size width height) windowFlags = do
    alloca $ \window -> do
    alloca $ \renderer -> do
      r <- sdlCreateWindowAndRenderer (fromIntegral width)
                                      (fromIntegral height)
                                      (toBitmask windowFlagToC windowFlags)
                                      window
                                      renderer
      if r == 0 then
        do
          win <- newForeignPtr sdlDestroyWindow_finalizer =<< peek window
          rend <- newForeignPtr sdlDestroyRenderer_finalizer =<< peek renderer
          return (win, rend)
       else
        (\err -> error $ "createWindowAndRenderer: " ++ show err) =<< getError

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


foreign import ccall unsafe "SDL_GetRenderDrawBlendMode"
  sdlGetRenderDrawBlendMode :: Ptr RendererStruct -> Ptr CInt -> IO CInt

getRenderDrawBlendMode :: Renderer -> IO BlendMode
getRenderDrawBlendMode renderer =
  withForeignPtr renderer $ \rp ->
    alloca $ \bm -> do
      r <- sdlGetRenderDrawBlendMode rp bm
      bm' <- peek bm
      handleErrorI "getRenderDrawBlendMode" r (const $ return (cIntToBlendMode bm'))


foreign import ccall unsafe "SDL_GetRenderDrawColor"
  sdlGetRenderDrawColor :: Ptr RendererStruct -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO CInt

getRenderDrawColor :: Renderer -> IO Color
getRenderDrawColor renderer = withForeignPtr renderer $ \r ->
    alloca $ \rp ->
    alloca $ \gp ->
    alloca $ \bp ->
    alloca $ \ap -> do
      ret <- sdlGetRenderDrawColor r rp gp bp ap
      handleErrorI "getRenderDrawColor" ret (const (Color <$> peek rp <*> peek gp <*> peek bp <*> peek ap))


{-foreign import ccall unsafe "SDL_GetRenderDriverInfo"
  sdlGetRenderDriverInfo :: CInt -> Ptr RendererInfoStruct -> IO CInt

getRenderDriverInfo :: Int -> IO RendererInfo
getRenderDriverInfo = undefined -- TODO-}


foreign import ccall unsafe "SDL_GetRenderTarget"
  sdlGetRenderTarget :: Ptr RendererStruct -> IO (Ptr TextureStruct)

getRenderTarget :: Renderer -> IO Texture
getRenderTarget renderer = withForeignPtr renderer $ \rp -> do
    r <- sdlGetRenderTarget rp
    newForeignPtr_ r


foreign import ccall unsafe "SDL_GetRenderer"
  sdlGetRenderer :: Ptr WindowStruct -> IO (Ptr RendererStruct)

getRenderer :: Window -> IO Renderer
getRenderer window = withForeignPtr window $ \wp -> do
    r <- sdlGetRenderer wp
    handleError "getRenderer" r (newForeignPtr sdlDestroyRenderer_finalizer)


{-getRendererInfo :: Renderer -> IO RendererInfo
getRendererInfo = undefined -- TODO-}


foreign import ccall unsafe "SDL_GetRendererOutputSize"
  sdlGetRendererOutputSize :: Ptr RendererStruct -> Ptr CInt -> Ptr CInt -> IO CInt

getRendererOutputSize :: Renderer -> IO (Int, Int)
getRendererOutputSize renderer = withForeignPtr renderer $ \rp ->
    alloca $ \wp ->
    alloca $ \hp -> do
      ret <- sdlGetRendererOutputSize rp wp hp
      handleErrorI "getRendererOutputSize" ret $ \_ -> do
        w <- peek wp
        h <- peek hp
        return (fromIntegral w, fromIntegral h)


foreign import ccall "SDL_GetTextureAlphaMod"
  sdlGetTextureAlphaMod :: Ptr TextureStruct -> Ptr Word8 -> IO CInt

getTextureAlphaMod :: Texture -> IO Word8
getTextureAlphaMod texture = withForeignPtr texture $ \tp ->
    alloca $ \ap -> do
      ret <- sdlGetTextureAlphaMod tp ap
      handleErrorI "getTextureAlphaMod" ret (const $ peek ap)


foreign import ccall "SDL_GetTextureBlendMode"
  sdlGetTextureBlendMode :: Ptr TextureStruct -> Ptr CInt -> IO CInt

getTextureBlendMode :: Texture -> IO BlendMode
getTextureBlendMode texture = withForeignPtr texture $ \tp ->
    alloca $ \bm -> do
      ret <- sdlGetTextureBlendMode tp bm
      bm' <- peek bm
      handleErrorI "getTextureBlendMode" ret (const $ return $ cIntToBlendMode bm')


foreign import ccall "SDL_GetTextureColorMod"
  sdlGetTextureColorMod :: Ptr TextureStruct -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO CInt

getTextureColorMod :: Texture -> IO (Int, Int, Int)
getTextureColorMod texture = withForeignPtr texture $ \tp ->
  alloca $ \rp ->
  alloca $ \gp ->
  alloca $ \bp -> do
    ret <- sdlGetTextureColorMod tp rp gp bp
    handleErrorI "getTextureColorMod" ret (const ((,,) <$> liftM fromIntegral (peek rp)
                                                       <*> liftM fromIntegral (peek gp)
                                                       <*> liftM fromIntegral (peek bp)))

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


foreign import ccall unsafe "SDL_RenderGetLogicalSize"
  sdlRenderGetLogicalSize :: Ptr RendererStruct -> Ptr CInt -> Ptr CInt -> IO ()

renderGetLogicalSize :: Renderer -> IO (Int, Int)
renderGetLogicalSize renderer = withForeignPtr renderer $ \rp ->
    alloca $ \wp ->
    alloca $ \hp -> do
      sdlRenderGetLogicalSize rp wp hp
      (,) <$> liftM fromIntegral (peek wp) <*> liftM fromIntegral (peek hp)


foreign import ccall unsafe "SDL_RenderGetScale"
  sdlRenderGetScale :: Ptr RendererStruct -> Ptr CFloat -> Ptr CFloat -> IO ()

renderGetScale :: Renderer -> IO (Float, Float)
renderGetScale renderer = withForeignPtr renderer $ \rp ->
    alloca $ \xp ->
    alloca $ \yp -> do
      sdlRenderGetScale rp xp yp
      (,) <$> liftM (\(CFloat f) -> f) (peek xp) <*> liftM (\(CFloat f) -> f) (peek yp)


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


--renderReadPixels = undefined


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


foreign import ccall unsafe "SDL_RenderSetScale"
  sdlRenderSetScale :: Ptr RendererStruct -> CFloat -> CFloat -> IO CInt

renderSetScale :: Renderer -> Float -> Float -> IO ()
renderSetScale renderer scaleX scaleY = withForeignPtr renderer $ \rp -> do
    ret <- sdlRenderSetScale rp (CFloat scaleX) (CFloat scaleY)
    handleErrorI "renderSetScale" ret (const $ return ())


foreign import ccall unsafe "SDL_RenderSetViewport"
  sdlRenderSetViewport :: Ptr RendererStruct -> Ptr Rect -> IO CInt

renderSetViewport :: Renderer -> Rect -> IO ()
renderSetViewport renderer rect =
  unwrapBool "renderSetViewport" $
  withForeignPtr renderer $ \r ->
  with rect $ \cr ->
  (== 0) <$> sdlRenderSetViewport r cr


foreign import ccall unsafe "SDL_RenderTargetSupported"
    sdlRenderTargetSupported :: Ptr RendererStruct -> IO CInt

renderTargetSupported :: Renderer -> IO Bool
renderTargetSupported renderer = withForeignPtr renderer $ \rp -> do
    ret <- sdlRenderTargetSupported rp
    return $ ret == 0


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
unlockTexture texture = withForeignPtr texture $ \tp -> sdlUnlockTexture tp


foreign import ccall unsafe "SDL_UpdateTexture"
  sdlUpdateTexture :: Ptr TextureStruct -> Ptr Rect -> Ptr a -> CInt -> IO CInt

updateTexture :: Texture -> Rect -> Ptr a -> Int -> IO ()
updateTexture texture rect pixels pitch =
  unwrapBool "updateTexture" $
  withForeignPtr texture $ \t ->
  with rect $ \r ->
  (== 0) <$> sdlUpdateTexture t r pixels (fromIntegral pitch)

--updateYUVTexture = undefined
