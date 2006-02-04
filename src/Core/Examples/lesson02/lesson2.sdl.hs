module Main where

import Graphics.Rendering.OpenGL
import Graphics.UI.SDL as SDL
import System.Exit
import System.Environment
import Control.Monad

main = withInit [InitVideo] $
       do progname <- getProgName
          createAWindow progname
          mainLoop

mainLoop :: IO ()
mainLoop = do event <- waitEvent
              quit <- case event of
                        VideoResize w h -> resizeGLScene w h >> drawGLScreen >> return False
                        VideoExpose -> drawGLScreen >> return False
                        KeyDown (Keysym SDLK_q _ _) -> return True
                        Quit -> return True
                        _ -> return False
              when (not quit) mainLoop

createAWindow name
    = do glSetAttribute glRedSize 5
         glSetAttribute glGreenSize 5
         glSetAttribute glBlueSize 5
         glSetAttribute glDepthSize 16
         glSetAttribute glDoubleBuffer 1
         setVideoMode width height 0 [OpenGL,Resizable]
         setCaption name ""
         depthFunc $= Just Less
         clearColor $= Color4 0 0 0 1
         let (w,h) = (fromIntegral width,fromIntegral height)
         loadIdentity
         perspective 45 (w/h) 0.1 100
    where width = 640
          height = 480

resizeGLScene :: Int -> Int -> IO ()
resizeGLScene w h = do
  setVideoMode w h 0 [OpenGL,Resizable]
  viewport $= (Position 0 0,Size (fromIntegral w) (fromIntegral h))
  matrixMode $= Projection
  loadIdentity
  let radio = (fromIntegral w)/(fromIntegral h)
  print radio
  perspective 45 radio 0.1 100
  matrixMode $= Modelview 0

drawGLScreen = do
  clear [ColorBuffer,DepthBuffer]
  loadIdentity
  translate $ (\(x,y,z) -> Vector3 x y z) polygonTrans
  renderPrimitive Polygon $ mapM_ (\(x,y,z) -> vertex$Vertex3 x y z) polygonPoints
  
  loadIdentity
  translate $ (\(x,y,z) -> Vector3 x y z) quadsTrans
  renderPrimitive Quads $ mapM_ (\(x,y,z) -> vertex$Vertex3 x y z) quadsPoints
  glSwapBuffers

polygonTrans :: (GLfloat,GLfloat,GLfloat)
polygonTrans = (-1.5,0,-6)

polygonPoints :: [(GLfloat,GLfloat,GLfloat)]
polygonPoints =
  [(0,1,0) ,(1,-1,0) ,(-1,-1,0)]

quadsTrans :: (GLfloat,GLfloat,GLfloat)
quadsTrans = (1.5,0,-6)

quadsPoints :: [(GLfloat,GLfloat,GLfloat)]
quadsPoints =
  [(-1,1,0) ,(1,1,0) ,(1,-1,0) ,(-1,-1,0)]

