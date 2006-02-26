module Main where

--FIXME: Add comments to this code.

import Graphics.Rendering.OpenGL as OpenGL
import Graphics.UI.SDL as SDL
import System.Exit
import System.Environment
import Data.IORef
import Control.Monad

step :: Fractional a => a
step = 0.5

main = withInit [InitVideo] $ do
  progName <- getProgName
  blendEquation $= FuncAdd
  blendFunc $= (OpenGL.SrcAlpha, OneMinusSrcAlpha)
  createAWindow progName

createAWindow name
    = do setVideoMode width height 0 [OpenGL,Resizable]
         rtri <- newIORef 0
         rquad <- newIORef 0
         stopped <- newIORef False
         depthFunc $= Just Less
         clearColor $= Color4 0 0 0 1
         loadIdentity
         perspective 45 (w/h) 0.1 100
         let loop = do drawGLScreen rtri rquad stopped
                       event <- pollEvent
                       quit <- case event of
                                 Quit -> return True
                                 KeyDown (Keysym SDLK_q _ _) -> return True
                                 KeyDown (Keysym SDLK_SPACE _ _) -> stopped $= True >> return False
                                 KeyUp (Keysym SDLK_SPACE _ _) -> stopped $= False >> return False
                                 VideoResize w h -> resizeGLScene w h >> return False
                                 _ -> return False
                       when (not quit) loop
         loop
      where width = 640
            height = 480
            (w,h) = (fromIntegral width, fromIntegral height)

resizeGLScene w h = do
  setVideoMode w h 0 [OpenGL,Resizable]
  viewport $= (Position 0 0,Size (fromIntegral w) (fromIntegral h))
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
  
  glSwapBuffers

polygonTrans :: (GLfloat,GLfloat,GLfloat)
polygonTrans = (-1.5,0,-6)

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
