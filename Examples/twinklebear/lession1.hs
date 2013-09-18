module Main where

import Control.Concurrent

import Graphics.UI.SDL as SDL



main :: IO ()
main =
  withInit [InitEverything] $
  withWindow "Hello World!" (Position 100 100) (Size 640 480) [WindowShown] $ \win ->
  withRenderer win (Device (-1)) [Accelerated, PresentVSync] $ \ren -> do
    bmp <- loadBMP "hello.bmp"
    tex <- createTextureFromSurface ren bmp
    renderClear ren
    renderCopy ren tex Nothing Nothing
    renderPresent ren

    threadDelay (10^6 * 2)
    return ()
