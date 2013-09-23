module Main where

import Control.Concurrent

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image as Image

-----------------------
-- Window parameters --

screenSize :: Size
screenSize = Size 640 480

screenPosition :: Position
screenPosition = Position 100 100

screenTitle :: String
screenTitle = "Lession 3"


------------------
-- Main program --

main :: IO ()
main =
  SDL.withInit [InitEverything] $
  Image.withInit [InitPNG] $
  withWindow screenTitle screenPosition screenSize [WindowShown] $ \win ->
  withRenderer win (Device (-1)) [Accelerated, PresentVSync] $ \renderer -> do
    background <- loadTexture "background.png" renderer
    image      <- loadTexture "image.png" renderer

    renderClear renderer
    Size bW bH <- queryTextureSize background
    renderTexture background renderer (Position 0 0)
    renderTexture background renderer (Position bW 0)
    renderTexture background renderer (Position 0 bH)
    renderTexture background renderer (Position bW bH)

    Size iW iH <- queryTextureSize image
    let x = sizeWidth screenSize `div` 2 - iW `div` 2
        y = sizeHeight screenSize `div` 2 - iH `div` 2
    renderTexture image renderer (Position x y)

    renderPresent renderer
    threadDelay (10^6 * 2)

-------------
-- Helpers --

loadTexture :: FilePath -> Renderer -> IO Texture
loadTexture path renderer = createTextureFromSurface renderer =<< load path

renderTexture :: Texture -> Renderer -> Position -> IO ()
renderTexture texture renderer (Position x y) = do
  Size w h <- queryTextureSize texture
  let dst = Rect x y w h
  renderCopy renderer texture Nothing (Just dst)

