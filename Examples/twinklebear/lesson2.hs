module Main where

import Control.Concurrent

import Graphics.UI.SDL as SDL

-----------------------
-- Window parameters --

screenSize :: Size
screenSize = Size 640 480

screenPosition :: Position
screenPosition = Position 100 100

screenTitle :: String
screenTitle = "Lession 2"


------------------
-- Main program --

main :: IO ()
main =
  withInit [InitEverything] $
  withWindow screenTitle screenPosition screenSize [WindowShown] $ \win ->
  withRenderer win (Device (-1)) [Accelerated, PresentVSync] $ \renderer -> do
    background <- loadTexture "background.bmp" renderer
    image      <- loadTexture "image.bmp" renderer

    renderClear renderer
    Size bW bH <- queryTexture background
    renderTexture background renderer (Position 0 0)
    renderTexture background renderer (Position bW 0)
    renderTexture background renderer (Position 0 bH)
    renderTexture background renderer (Position bW bH)

    Size iW iH <- queryTexture image
    let x = sizeWidth screenSize `div` 2 - iW `div` 2
        y = sizeHeight screenSize `div` 2 - iH `div` 2
    renderTexture image renderer (Position x y)

    renderPresent renderer
    threadDelay (10^6 * 2)

-------------
-- Helpers --

loadTexture :: FilePath -> Renderer -> IO Texture
loadTexture path renderer = createTextureFromSurface renderer =<< loadBMP path

renderTexture :: Texture -> Renderer -> Position -> IO ()
renderTexture texture renderer (Position x y) = do
  Size w h <- queryTexture texture
  let dst = Rect x y w h
  renderCopy renderer texture Nothing (Just dst)

