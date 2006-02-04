module Main where

import Graphics.Rendering.OpenGL as OpenGL
import Graphics.SDL as SDL

import System.Environment
import System.Random
import Data.IORef
import Control.Monad
import Objects

data Star
    = Star
    { red, green, blue :: IORef GLfloat
    , dist, angle :: IORef GLfloat
    } deriving Show

main = withInit [InitVideo] $
       do progName <- getProgName
          setVideoMode width height 0 [OpenGL,Resizeable,DoubleBuf]
          setCaption progName ""
          resizeGLScene width height
          initAndRun
    where width = 640
          height = 480

numStars = 25
seed = 623025
step = 0.1

predStep x = x - step
succStep x = x + step

initDist loop
    = loop * 1 / fromIntegral numStars * 5

initAndRun =
    do starImage <- loadBMP "Data/lesson9/Star.bmp"
       black <- mapRGB (surfaceGetPixelFormat starImage) 0x00 0x00 0x00
       setColorKey starImage [] black
       obj <- loadObject starImage $ textureFilter Texture2D $= ((Linear', Nothing),Linear')
       texture Texture2D $= Enabled
       clearColor $= Color4 0 0 0 0
       blend $= Enabled
       blendFunc $= (OpenGL.SrcAlpha,One)
       spinRef <- newIORef 0
       tiltRef <- newIORef (90::GLfloat)
       zoomRef <- newIORef ((-15):: GLfloat)
       let loop stars
               = do zoom <- readIORef zoomRef
                    tilt <- readIORef tiltRef
                    delay 10
                    newStars <- drawGLScreen (objTexId obj) stars spinRef zoom tilt
                    event <- pollEvent
                    quit <- case event of
                              Quit -> return True
                              KeyDown (Keysym SDLK_q _ _) -> return True
                              VideoResize w h -> resizeGLScene w h >> return False
                              _ -> return False
                    when (not quit) (loop newStars)
       loop (createStars (mkStdGen seed) 0)
    where createStars rn loop
              = let (r,rn') = randomR (0,1)  rn
                    (g,rn'') = randomR (0,1) rn'
                    (b,rn''') = randomR (0,1) rn''
                    d = initDist loop
                in Star r g b d 0:createStars rn''' (succ loop)


drawGLScreen texName stars colors spinRef zoom tilt
    = do clear [ColorBuffer,DepthBuffer]
         textureBinding Texture2D $= Just texName
         mapM_ (\star ->
                do spin <- get spinRef
                   loadIdentity
                   translate $ Vector3 0 0 zoom
                   rotate tilt $ Vector3 1 0 0
                   rotate (angle star) $ Vector3 0 1 0
                   translate $ Vector3 (dist star) 0 0
                   rotate (negate (angle star)) $ Vector3 0 1 0
                   rotate (negate tilt) $ Vector3 1 0 0
                   -- main star
                   rotate spin $ Vector3 0 0 1
                   -- [r,g,b] <- mapM readIORef [red star, green star, blue star]
                   color $ Color3 (red star) (green star) (blue star)
                   renderPrimitive Quads $
                     mapM_ (\((c1,c2),(x,y,z)) ->
                                do texCoord $ TexCoord2 c1 c2
                                   vertex $ Vertex3 x y z
                           ) quadPoints
                   modifyIORef spinRef succStep
               ) firstStars
         glSwapBuffers
         return (mkNewStars starSupply (zip firstStars [0..]))
    where (firstStars,starSupply) = splitAt numStars stars
          mkNewStars gen [] = gen
          mkNewStars gen@(s:ss) ((star,loop):stars)
              | dist star <= 0 = s{angle = angle star}:mkNewStars ss stars
              | otherwise = star{dist = predStep (dist star)
                                ,angle = angle star + (min (fromIntegral numStars) loop) * 1 / fromIntegral numStars * 1
                                } : mkNewStars gen stars
                              

resizeGLScene w h
    = do setVideoMode w h 0 [OpenGL,Resizeable]
         viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))
         matrixMode $= Projection
         loadIdentity
         perspective 45 radio 0.1 100
         matrixMode $= Modelview 0
    where radio = fromIntegral w / fromIntegral h

quadPoints :: [((GLfloat,GLfloat),(GLfloat,GLfloat,GLfloat))]
quadPoints
    = [((0,0),(-1,-1,0))
      ,((1,0),(1,-1,0))
      ,((1,1),(1,1,0))
      ,((0,1),(-1,1,0))]
