module Main where

--FIXME: Add comments to this code.

import Graphics.Rendering.OpenGL as OpenGL
import Graphics.SDL as SDL
import System.Exit
import System.Environment
import Data.IORef
import Control.Monad

import Objects

step = 0.1

main = withInit [InitVideo] $ do
  progName <- getProgName
  createAWindow progName

myInit :: IO TextureObject
myInit = do
  clearColor $= Color4 0 0 1 1
  shadeModel $= Smooth

  surface <- loadBMP "Data/lesson6/NeHe.bmp"
  white <- mapRGB (surfaceGetPixelFormat surface) 0x00 0x00 0x00
  setColorKey surface [] white
  obj <- loadObject surface

  blend $= Enabled
  blendEquation $= FuncAdd
  --blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  blendFunc $= (OpenGL.SrcAlpha,OneMinusSrcAlpha)
  textureFunction $= Decal
  depthFunc $= Just Less
 
  return (objTexId obj)

createAWindow name 
    = do setVideoMode width height 0 [OpenGL,Resizeable]
         rtri <- newIORef 0
         rquad <- newIORef 0
         stopped <- newIORef False
         texName <- myInit
         loadIdentity
         perspective 45 (w/h) 0.1 100
         let loop = do drawGLScreen rtri rquad stopped texName
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
  setVideoMode w h 0 [OpenGL,Resizeable]
  viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))
  matrixMode $= Projection
  loadIdentity
  let radio = (fromIntegral w)/(fromIntegral h)
  perspective 45 radio 0.1 100
  matrixMode $= Modelview 0

drawGLScreen :: IORef GLfloat -> IORef GLfloat -> IORef Bool -> TextureObject ->IO ()
drawGLScreen rtri rquad stopped tex = do
  clear [ColorBuffer,DepthBuffer]
  loadIdentity

  stop <- get stopped
  
  translate $ (\(x,y,z) -> Vector3 x y z) quadsTrans
  rot <- get rquad
  rotate rot $ Vector3 (1::GLfloat) 0 0
  rotate rot $ Vector3 0 (1::GLfloat) 0
  rotate rot $ Vector3 0 0 (1::GLfloat)
  rquad $=
    if stop
      then rot
      else rot+step

  currentColor $= Color4 1 1 1 1

  texture Texture2D $= Enabled
  textureBinding Texture2D $= Just tex
  renderPrimitive Quads $ mapM_ ( mapM_ (\((cx,cy),(x,y,z)) -> do
    texCoord (TexCoord2 cx cy)
    vertex$Vertex3 x y z)) quadsPoints
  texture Texture2D $= Disabled

  err <- get errors
  when (not $ null err) (print err)

  glSwapBuffers

quadsTrans :: (GLfloat,GLfloat,GLfloat)
quadsTrans = (0,0,-5)

avg :: (Fractional a) => [a] -> a
avg lst = sum lst / fromIntegral (length lst)

getZVal :: [((GLfloat,GLfloat),(GLfloat,GLfloat,GLfloat))] -> GLfloat
getZVal = avg . worker
    where worker [] = []
	  worker (((_,_),(_,_,z)):xs) = z:worker xs

quadsPoints :: [[((GLfloat,GLfloat),(GLfloat,GLfloat,GLfloat))]]
quadsPoints =
  [
   [((0,0),(1,-1,-1)),((1,0),(-1,-1,-1)),((1,1),(-1,1,-1)) ,((0,1),(1,1,-1)) ] -- Back
  ,[((1,1),(-1,1,1)) ,((0,1),(-1,1,-1)) ,((0,0),(-1,-1,-1)),((1,0),(-1,-1,1))] -- Left
  ,[((0,0),(1,1,1))  ,((1,0),(-1,1,1))  ,((1,1),(-1,1,-1)) ,((0,1),(1,1,-1)) ] -- Top
  ,[((1,1),(1,1,-1)) ,((0,1),(1,1,1))   ,((0,0),(1,-1,1))  ,((1,0),(1,-1,-1))] -- Righ
  ,[((0,0),(1,-1,1)) ,((1,0),(-1,-1,1)) ,((1,1),(-1,-1,-1)),((0,1),(1,-1,-1))] -- Bottom
  ,[((1,1),(1,1,1))  ,((0,1),(-1,1,1))  ,((0,0),(-1,-1,1)) ,((1,0),(1,-1,1)) ] -- Front
  ]

