module Main where

--FIXME: Add comments to this code.

import Graphics.UI.GLUT
import System.Exit
import Data.IORef
import Data.Maybe
import Control.Monad
import BmpLoader
import Control.Concurrent

step = 0.1

withBmpFile :: FilePath -> ((GLsizei,GLsizei,PixelData (Color4 GLubyte)) -> IO ()) -> IO ()
withBmpFile path act = do
  s@(w,h,d) <- loadBitmap path (Just [(0xff,0xff,0xff)])
  putStrLn ("Width/Height: "++show w++"/"++show h)
  act s


main = do
  (progName,_) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered,WithDepthBuffer,RGBMode]
  createAWindow progName
  loadIdentity
  mainLoop

myInit :: IO (Maybe TextureObject)
myInit = do
  clearColor $= Color4 0 0 0 1
  --shadeModel $= Smooth
  rowAlignment Unpack $= 1
                                                                                                                            
  exts <- get glExtensions
  mbTexName <- if "GL_EXT_texture_object" `elem` exts
                  then liftM listToMaybe $ genObjectNames 1
                  else return Nothing
  when (isJust mbTexName) $ textureBinding Texture2D $= mbTexName
                                                                                                                            
  textureWrapMode Texture2D S $= (Repeated, Repeat)
  textureWrapMode Texture2D T $= (Repeated, Repeat)
  textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
  --withCheckImage checkImageSize 0x8 (\c -> Color4 c c c 255) $
  withBmpFile "Data/lesson6/NeHe.bmp" $ (\(w,h,d) ->
  --withBmpFile "Data/lesson6/glass.bmp" $ (\(w,h,d) ->
     texImage2D Nothing NoProxy 0  RGBA' (TextureSize2D w h) 0 d)

  blendEquation $= Just FuncAdd
  --blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  blendFunc $= (SrcAlpha,OneMinusSrcAlpha)
  --textureFunction $= Decal
  depthFunc $= Nothing
  

  return mbTexName

createAWindow name = do
  createWindow name
  rtri <- newIORef 0
  rquad <- newIORef 0
  stopped <- newIORef False
  mbTexName <- myInit
  displayCallback $= (drawGLScreen rtri rquad stopped mbTexName)
  idleCallback $= Just (drawGLScreen rtri rquad stopped mbTexName)
  reshapeCallback $= Just resizeGLScene
  keyboardMouseCallback $= Just (keyPressed stopped)
  windowSize $= Size 640 480
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
5~
drawGLScreen :: IORef GLfloat -> IORef GLfloat -> IORef Bool -> Maybe TextureObject ->IO ()
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
  textureBinding Texture2D $= tex
  --renderPrimitive Quads $ 
  depthMask $= Disabled
  renderPrimitive Quads $ mapM_ ( mapM_ (\((cx,cy),(x,y,z)) -> do
    texCoord (TexCoord2 cx cy)
    --color $ Color4 x y z 0.7
    vertex$Vertex3 x y z)) quadsPoints
  depthMask $= Enabled
  texture Texture2D $= Disabled

  err <- get errors
  if not $ null err
     then print err
     else return ()

  swapBuffers

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

keyPressed stopped (Char '\ESC') Down _ _ = exitWith ExitSuccess
keyPressed stopped (Char ' ') Down _ _ = stopped $= True
keyPressed stopped (Char ' ') Up _ _ = stopped $= False
keyPressed stopped (Char 'b') Down _ _ = do blendEquation $= Nothing
					    depthFunc $= Just Less
keyPressed stopped (Char 'b') Up _ _ = do blendEquation $= Just FuncAdd
					  depthFunc $= Nothing
keyPressed stopped x Down _ _ = putStrLn (show x)
keyPressed stopped _ _ _ _ = return ()
