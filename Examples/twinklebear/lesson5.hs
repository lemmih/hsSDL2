{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Concurrent
import Control.Monad
import System.Exit
import Data.Function

import Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Keycode as Key
import Graphics.UI.SDL.Image as Image

-----------------------
-- Window parameters --

screenSize :: Size
screenSize = Size 640 480

screenPosition :: Position
screenPosition = Position 100 100

screenTitle :: String
screenTitle = "Lession 5"


------------------
-- Main program --

main :: IO ()
main =
  SDL.withInit [InitEverything] $
  Image.withInit [InitPNG] $
  withWindow screenTitle screenPosition screenSize [WindowShown] $ \win ->
  withRenderer win (Device (-1)) [Accelerated, PresentVSync] $ \renderer -> do
    image      <- loadTexture "lesson5.png" renderer

    let iW = 100
        iH = 100
        x  = sizeWidth screenSize `div` 2 - iW `div` 2
        y  = sizeHeight screenSize `div` 2 - iH `div` 2

    let mkClip i = Rect
          { rectX = i `div` 2 * iW
          , rectY = i `mod` 2 * iH
          , rectW = iW
          , rectH = iH }

    flip fix 0 $ \loop useClip -> do
      mbEvent <- pollEvent
      case fmap eventData mbEvent of
        Just Quit                              -> exitSuccess
        Just Keyboard{ keyMovement = KeyDown, keySym = Keysym{..} }
          | keyKeycode == Key.Number1          -> loop 0
          | keyKeycode == Key.Number2          -> loop 1
          | keyKeycode == Key.Number3          -> loop 2
          | keyKeycode == Key.Number4          -> loop 3
          | keyKeycode == Key.Escape           -> exitSuccess
        _otherwise                             -> do
          renderClear renderer
          renderTexture image renderer (Position x y) (Just $ mkClip useClip)
          renderPresent renderer
          loop useClip



-------------
-- Helpers --

loadTexture :: FilePath -> Renderer -> IO Texture
loadTexture path renderer = createTextureFromSurface renderer =<< load path

renderTexture :: Texture -> Renderer -> Position -> Maybe Rect -> IO ()
renderTexture texture renderer (Position x y) mbClip = do
  dst <- case mbClip of
            Nothing -> do
              Size w h <- queryTextureSize texture
              return $ Rect x y w h
            Just (Rect _x _y w h) ->
              return $ Rect x y w h
  renderCopy renderer texture mbClip (Just dst)

