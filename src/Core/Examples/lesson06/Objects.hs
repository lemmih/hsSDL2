module Objects
    ( Object(..)
    , loadObject
    , withObject
    , drawObject
    ) where


import Graphics.Rendering.OpenGL as OpenGL
import Graphics.SDL as SDL

import Data.Bits (shiftL)
import Control.Monad

-- TODO: Add support for delaying the loading of texture and/or grouping several images into one texture.
data Object
    = Object
    { objTexId :: TextureObject
    , objTexWidth :: GLfloat    -- Width of the original image in pixels.
    , objTexHeight :: GLfloat   -- Height of the original image in pixels.
    , objTexX :: GLfloat        -- Texture X pos.
    , objTexY :: GLfloat        -- Texture Y pos
    } 


withObject :: Object -> (TextureObject -> IO a) -> IO a
withObject obj action = action (objTexId obj)


loadObject :: Surface -> IO () -> IO Object
loadObject surface conf
    = do image <- createRGBSurfaceEndian [SWSurface] w h 32
         flags <- surfaceGetFlags surface
         alpha <- pixelFormatGetAlpha (surfaceGetPixelFormat surface)
         when (SDL.SrcAlpha `elem` flags) (setAlpha surface [] 0 >> return ())
         blitSurface surface (Just area) image (Just area)
         when (SDL.SrcAlpha `elem` flags) (setAlpha surface flags alpha >> return ())
         texName <- fmap head (genObjectNames 1)
         textureBinding Texture2D $= Just texName
         conf
         pixels <- surfaceGetPixels image
         texImage2D Nothing NoProxy 0 RGBA' (TextureSize2D (fromIntegral w) (fromIntegral h))
                        0 (PixelData RGBA UnsignedByte pixels)
         return (Object texName (fromIntegral surfaceW) (fromIntegral surfaceH)
                 (fromIntegral surfaceW / fromIntegral w) (fromIntegral surfaceH / fromIntegral h))
    where surfaceW = surfaceGetWidth surface
          surfaceH = surfaceGetHeight surface
          w = powerOfTwo surfaceW
          h = powerOfTwo surfaceH
          area = Rect 0 0 surfaceW surfaceH

powerOfTwo n = until (>=n) (`shiftL` 1) 1


drawObject :: Object -> IO ()
drawObject (Object tex w h texX texY)
    = do texture Texture2D $= Enabled
         textureBinding Texture2D $= Just tex
         renderPrimitive TriangleStrip
             $ do texCoord (TexCoord2 0 (0::GLfloat))
                  vertex (Vertex2 0 (0::GLint))
                  texCoord (TexCoord2 texX 0)
                  vertex (Vertex2 (w) 0)
                  texCoord (TexCoord2 0 texY)
                  vertex (Vertex2 0 (h))
                  texCoord (TexCoord2 texX texY)
                  vertex (Vertex2 (w) (h))

