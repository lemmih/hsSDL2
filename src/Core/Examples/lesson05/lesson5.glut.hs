module Main where

--FIXME: Add comments to this code.

import Graphics.UI.GLUT
import System.Exit
import Data.IORef

step = 0.1


main = do
  (progName,_) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered,WithDepthBuffer,RGBMode]
  createAWindow progName
  blendEquation $= FuncAdd
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  mainLoop

createAWindow name = do
  createWindow name
  rtri <- newIORef 0
  rquad <- newIORef 0
  stopped <- newIORef False
  displayCallback $= (drawGLScreen rtri rquad stopped)
  idleCallback $= Just (drawGLScreen rtri rquad stopped)
  reshapeCallback $= Just resizeGLScene
  keyboardMouseCallback $= Just (keyPressed stopped)
  windowSize $= Size 640 480
  depthFunc $= Just Less
  clearColor $= Color4 0 0 0 1
  x <- get screenSize
  let (width,height) = case x of Size w h -> (fromIntegral w,fromIntegral h)
  loadIdentity
  perspective 45 (width/height) 0.1 100

resizeGLScene s@(Size w h) = do
  viewport $= (Position 0 0,s)
  matrixMode $= Projection
  loadIdentity
  let radio = (fromIntegral w)/(fromIntegral h)
  perspective 45 radio 0.1 100
  matrixMode $= Modelview 0

drawGLScreen :: IORef GLfloat -> IORef GLfloat -> IORef Bool -> IO ()
drawGLScreen rtri rquad stopped= do
  clear [ColorBuffer,DepthBuffer]
  loadIdentity

  stop <- get stopped
  
  translate $ (\(x,y,z) -> Vector3 x y z) polygonTrans
  rot <- get rtri
  rotate rot $ Vector3 0 (1::GLfloat) 0
  rotate rot $ Vector3 (1::GLfloat) 0 0
  rtri $=
    if stop
      then rot
      else rot+step
  mapM_ (\l -> do
    renderPrimitive Polygon $ mapM_ (\((r,g,b),(x,y,z)) -> do
      currentColor $= Color4 r g b 1
      vertex$Vertex3 x y z) l
    ) polygonPoints
  
  loadIdentity
  translate $ (\(x,y,z) -> Vector3 x y z) quadsTrans
  rot <- get rquad
  rotate rot $ Vector3 (1::GLfloat) 0 0
  rotate rot $ Vector3 0 (1::GLfloat) 0
  rquad $=
    if stop
      then rot
      else rot-step
  currentColor $= Color4 0.5 0.5 1 0.5
  renderPrimitive Quads $ mapM_ (\((r,g,b),l) -> do
    currentColor $= Color4 r g b 0.5
    mapM_ (\(x,y,z) -> vertex$Vertex3 x y z) l
    ) quadsPoints
  
  swapBuffers

polygonTrans :: (GLfloat,GLfloat,GLfloat)
polygonTrans = (2.5,0,-6)

polygonPoints :: [[((GLfloat,GLfloat,GLfloat),(GLfloat,GLfloat,GLfloat))]]
polygonPoints =
  [ [((1,0,0),(0,1,0))
    ,((0,1,0),(-1,-1,1))
    ,((0,0,1),(1,-1,1))] -- Front
  , [((1,0,0),(0,1,0))
    ,((0,0,1),(1,-1,1))
    ,((0,1,0),(1,-1,-1))] -- Right
  , [((1,0,0),(0,1,0))
    ,((0,1,0),(1,-1,-1))
    ,((0,0,1),(-1,-1,-1))] -- Back
  , [((1,0,0),(0,1,0))
    ,((0,0,1),(-1,-1,-1))
    ,((0,1,0),(-1,-1,1))] -- Left
  , [((0,0.5,0.5),(0,-1,0))
    ,((0,0,1),(-1,-1,-1))
    ,((0,1,0),(1,-1,-1))] -- Bottom Back
  , [((0,0.5,0.5),(0,-1,0))
    ,((0,0,1),(1,-1,1))
    ,((0,1,0),(1,-1,-1))] -- Bottom Right
  , [((0,0.5,0.5),(0,-1,0))
    ,((0,0,1),(-1,-1,-1))
    ,((0,1,0),(-1,-1,1))] -- Bottom Left
  , [((0,0.5,0.5),(0,-1,0))
    ,((0,1,0),(-1,-1,1))
    ,((0,0,1),(1,-1,1))] -- Bottom Front
  ]

quadsTrans :: (GLfloat,GLfloat,GLfloat)
quadsTrans = (1.5,0,-6)

quadsPoints :: [((GLfloat,GLfloat,GLfloat),[(GLfloat,GLfloat,GLfloat)])]
quadsPoints =
  [((0,1,0),  [(1,1,-1) ,(-1,1,-1) ,(-1,1,1)  ,(1,1,1)  ]) -- Top
  ,((1,0.5,0),[(1,-1,1) ,(-1,-1,1) ,(-1,-1,-1),(1,-1,-1)]) -- Bottom
  ,((1,0,0),  [(1,1,1)  ,(-1,1,1)  ,(-1,-1,1) ,(1,-1,1) ]) -- Front
  ,((1,1,0),  [(1,-1,-1),(-1,-1,-1),(-1,1,-1) ,(1,1,-1) ]) -- Back
  ,((0,0,1),  [(-1,1,1) ,(-1,1,-1) ,(-1,-1,-1),(-1,-1,1)]) -- Left
  ,((0,0,1),  [(1,1,-1) ,(1,1,1)   ,(1,-1,1)  ,(1,-1,-1)]) -- Right
  ]

keyPressed stopped (Char '\ESC') Down _ _ = exitWith ExitSuccess
keyPressed stopped (Char ' ') Down _ _ = stopped $= True
keyPressed stopped (Char ' ') Up _ _ = stopped $= False
keyPressed stopped x Down _ _ = putStrLn (show x)
keyPressed stopped _ _ _ _ = return ()
