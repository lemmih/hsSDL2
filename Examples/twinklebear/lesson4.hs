module Main where

import Control.Concurrent
import Control.Monad
import System.Exit

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image as Image

-----------------------
-- Window parameters --

screenSize :: Size
screenSize = Size 640 480

screenPosition :: Position
screenPosition = Position 100 100

screenTitle :: String
screenTitle = "Lession 4"


------------------
-- Main program --

main :: IO ()
main =
  SDL.withInit [InitEverything] $
  Image.withInit [InitPNG] $
  withWindow screenTitle screenPosition screenSize [WindowShown] $ \win ->
  withRenderer win (Device (-1)) [Accelerated, PresentVSync] $ \renderer -> do
    image      <- loadTexture "lesson4.png" renderer

    Size iW iH <- queryTextureSize image
    let x = sizeWidth screenSize `div` 2 - iW `div` 2
        y = sizeHeight screenSize `div` 2 - iH `div` 2
    renderTexture image renderer (Position x y)

    forever $ do
      mbEvent <- pollEvent
      print mbEvent
      case fmap eventData mbEvent of
        Just Quit                              -> exitSuccess
        Just Keyboard{ keyMovement = KeyDown } -> exitSuccess
        Just MouseButton{}                     -> exitSuccess
        _otherwise                        -> return ()

      renderClear renderer
      renderTexture image renderer (Position x y)
      renderPresent renderer

-------------
-- Helpers --

loadTexture :: FilePath -> Renderer -> IO Texture
loadTexture path renderer = createTextureFromSurface renderer =<< load path

renderTexture :: Texture -> Renderer -> Position -> IO ()
renderTexture texture renderer (Position x y) = do
  Size w h <- queryTextureSize texture
  let dst = Rect x y w h
  renderCopy renderer texture Nothing (Just dst)

