-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.SDL.Framerate
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Graphics.UI.SDL.Framerate
    ( new
    , init
    , set
    , get
    , delay
    , FPSManager
    ) where

import Foreign as Foreign hiding (new)
import Foreign.C

import Prelude hiding (init)

data FPSManagerStruct
type FPSManager = ForeignPtr FPSManagerStruct

sizeOfManager :: Int
sizeOfManager = 16

new :: IO FPSManager
new  = do manager <- Foreign.mallocBytes sizeOfManager >>= newForeignPtr finalizerFree
          init manager
          return manager

-- void SDL_initFramerate(FPSmanager * manager);
foreign import ccall unsafe "SDL_initFramerate" sdlInitFramerate :: Ptr FPSManagerStruct -> IO ()
init :: FPSManager -> IO ()
init manager
    = withForeignPtr manager sdlInitFramerate


-- int SDL_setFramerate(FPSmanager * manager, int rate);
foreign import ccall unsafe "SDL_setFramerate" sdlSetFramerate :: Ptr FPSManagerStruct -> Int -> IO Int
set :: FPSManager -> Int -> IO Bool
set manager hz
    = withForeignPtr manager $ \fpsPtr ->
      do ret <- sdlSetFramerate fpsPtr hz
         case ret of
           (-1) -> return False
           _    -> return True

-- int SDL_getFramerate(FPSmanager * manager);
foreign import ccall unsafe "SDL_getFramerate" sdlGetFramerate :: Ptr FPSManagerStruct -> IO Int
get :: FPSManager -> IO Int
get manager = withForeignPtr manager sdlGetFramerate

-- void SDL_framerateDelay(FPSmanager * manager);
foreign import ccall unsafe "SDL_framerateDelay" sdlFramerateDelay :: Ptr FPSManagerStruct -> IO ()
delay :: FPSManager -> IO ()
delay manager = withForeignPtr manager sdlFramerateDelay


