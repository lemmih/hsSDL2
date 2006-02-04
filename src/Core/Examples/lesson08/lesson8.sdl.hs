
{-# OPTIONS -i.. #-}
module Main where


import Graphics.Rendering.OpenGL as OpenGL
import Graphics.SDL as SDL hiding (flip)
import Data.IORef
import System.Environment
import Control.Monad
import Objects

step = 0.1

succStep x = x + step
predStep x = x - step

main = withInit [InitVideo] $
       do progName <- getProgName
          setVideoMode width height 0 [OpenGL,Resizeable]
          setCaption progName ""
          resizeGLScene width height
          initAndRun
    where width = 640
          height = 480

initAndRun :: IO ()
initAndRun =
    do (textures,image,xrot,yrot,xspeed,yspeed) <- myInit
       setVideoMode width height 0 [OpenGL,Resizeable]
       resizeGLScene width height
       let loop = do delay 10
                     event <- pollEvent
                     drawGLScreen textures image xrot yrot xspeed yspeed
                     quit <- case event of
                               Quit -> return True
                               KeyDown (Keysym SDLK_q _ _) -> return True
                               KeyDown (Keysym SDLK_LEFT _ _) -> modifyIORef yspeed predStep >> return False
                               KeyDown (Keysym SDLK_RIGHT _ _) -> modifyIORef yspeed succStep >> return False
                               KeyDown (Keysym SDLK_UP _ _) -> modifyIORef xspeed predStep >> return False
                               KeyDown (Keysym SDLK_DOWN _ _) -> modifyIORef xspeed succStep >> return False
                               KeyDown (Keysym SDLK_f _ _) -> modifyIORef image (flip mod 3 . succ) >> return False
                               VideoResize w h -> resizeGLScene w h >> return False
                               _ -> return False
                     when (not quit) loop
       loop
    where width = 640
          height = 480

drawGLScreen :: [TextureObject] -> IORef Int -> IORef GLfloat -> IORef GLfloat -> IORef GLfloat -> IORef GLfloat -> IO ()
drawGLScreen textures imageRef xrotRef yrotRef xspeedRef yspeedRef
    = do clear [ColorBuffer, DepthBuffer]
         loadIdentity
         xrot <- get xrotRef
         yrot <- get yrotRef
         xspeed <- get xspeedRef
         yspeed <- get yspeedRef
         filter <- get imageRef
         translate $ Vector3 0 0 (-5::GLfloat)
         rotate xrot $ Vector3 (1::GLfloat) 0 0
         rotate yrot $ Vector3 (0::GLfloat) 1 0
         texture Texture2D $= Enabled
         textureBinding Texture2D $= Just (textures!!filter)
         renderPrimitive Quads $
           mapM_ (\((n1,n2,n3),points) ->
                      do normal $ Normal3 n1 n2 n3
                         mapM_ (\((coord1,coord2),(x,y,z)) ->
                                    do texCoord $ TexCoord2 coord1 coord2
                                       vertex $ Vertex3 x y z
                               ) points
                 ) quadsPoints
         xrotRef $= xrot+xspeed
         yrotRef $= yrot+yspeed
         glSwapBuffers

myInit :: IO ([TextureObject], IORef Int,IORef GLfloat, IORef GLfloat, IORef GLfloat, IORef GLfloat)
myInit = do image <- loadBMP "Data/lesson8/glass.bmp"
            tex1 <- loadObject image $ textureFilter Texture2D $= ((Nearest, Nothing),Nearest)
            tex2 <- loadObject image $ textureFilter Texture2D $= ((Linear', Nothing),Linear')
            tex3 <- loadObject image $ do generateMipmap Texture2D $= Enabled
                                          textureFilter Texture2D $= ((Linear', Just Nearest),Linear')
            [xrot,yrot,xspeed,yspeed] <- mapM newIORef [0,0,0,0]
            image <- newIORef 0
            blend $= Enabled
            blendEquation $= FuncAdd
            blendFunc $= (OpenGL.SrcAlpha,OneMinusSrcAlpha)
            depthFunc $= Just Less
            clearColor $= Color4 0 0 1 1
            shadeModel $= Smooth
            return (map objTexId [tex1,tex2,tex3],image,xrot,yrot,xspeed,yspeed)

resizeGLScene w h
    = do setVideoMode w h 0 [OpenGL,Resizeable]
         viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))
         matrixMode $= Projection
         loadIdentity
         perspective 45 radio 0.1 100
         matrixMode $= Modelview 0
    where radio = fromIntegral w / fromIntegral h


quadsPoints :: [((GLfloat,GLfloat,GLfloat),[((GLfloat,GLfloat),(GLfloat,GLfloat,GLfloat))])]
quadsPoints =
    [
     ((0,0,1)               -- Font
      ,[((0,0),(-1,-1,1))
       ,((1,0),(1,-1,1))
       ,((1,1),(1,1,1))
       ,((0,1),(-1,1,1))])
     ,((0,0,-1)             -- Back
      ,[((1,0),(-1,-1,-1))
       ,((1,1),(-1,1,-1))
       ,((0,1),(1,1,-1))
       ,((0,0),(1,-1,-1))])
     ,((0,1,0)              -- Top
      ,[((0,1),(-1,1,-1))
       ,((0,0),(-1,1,1))
       ,((1,0),(1,1,1))
       ,((1,1),(1,1,-1))])
     ,((0,-1,0)             -- Bottom
      ,[((1,1),(-1,-1,-1))
       ,((0,1),(1,-1,-1))
       ,((0,0),(1,-1,1))
       ,((1,0),(-1,-1,1))])
     ,((1,0,0)              -- Right
      ,[((1,0),(1,-1,-1))
       ,((1,1),(1,1,-1))
       ,((0,1),(1,1,1))
       ,((0,0),(1,-1,1))])
     ,((-1,0,0)             -- Left
      ,[((0,0),(-1,-1,-1))
       ,((1,0),(-1,-1,1))
       ,((1,1),(-1,1,1))
       ,((0,1),(-1,1,-1))])
    ]
