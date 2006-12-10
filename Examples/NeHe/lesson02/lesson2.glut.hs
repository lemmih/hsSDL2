module Main where

--FIXME: Add comments to this code.

import Graphics.UI.GLUT
import System.Exit

main = do
  (progName,_) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered,WithDepthBuffer]
  createAWindow progName
  mainLoop

createAWindow name = do
  createWindow name
  displayCallback $= drawGLScreen
  reshapeCallback $= Just resizeGLScene
  keyboardMouseCallback $= Just keyPressed
  windowSize $= Size 640 480
  depthFunc $= Just Less
  clearColor $= Color4 0 0 0 1
  x <- get screenSize
  let (width,height) = case x of Size w h -> (fromIntegral w,fromIntegral h)
  loadIdentity
  perspective 45 (width/height) 0.1 100

resizeGLScene s@(Size w h) = do
  print s
  viewport $= (Position 0 0,s)
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
  swapBuffers

polygonTrans :: (GLfloat,GLfloat,GLfloat)
polygonTrans = (-1.5,0,-6)

polygonPoints :: [(GLfloat,GLfloat,GLfloat)]
polygonPoints =
  [(0,1,0) ,(1,-1,0) ,(-1,-1,0)]

quadsTrans :: (GLfloat,GLfloat,GLfloat)
quadsTrans = (1.5,0,-6)

--quadsPoint :: (GLfloat,GLfloat,GLfloat)
quadsPoints :: [(GLfloat,GLfloat,GLfloat)]
quadsPoints =
  [(-1,1,0) ,(1,1,0) ,(1,-1,0) ,(-1,-1,0)]

keyPressed (Char '\ESC') Down _ _ = exitWith ExitSuccess
keyPressed x Down _ _ = putStrLn (show x)
keyPressed _ _ _ _ = return ()
