#include "SDL.h"
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.SDL.Joystick
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Graphics.UI.SDL.Joystick
    ( countAvailable
    , tryName
    , name
    , tryOpen
    , open
    , opened
    , index
    , axesAvailable
    , ballsAvailable
    , hatsAvailable
    , buttonsAvailable
    , getAxis
    , getHat
    , getButton
    , getBall
    , close
    ) where

import Foreign
import Foreign.C

import Graphics.UI.SDL.General
import Graphics.UI.SDL.Utilities
import Graphics.UI.SDL.Types

type JoystickIndex = Int

--int SDL_NumJoysticks(void);
-- | Counts the number of joysticks attached to the system.
foreign import ccall unsafe "SDL_NumJoysticks" countAvailable :: IO Int

-- const char *SDL_JoystickName(int index);
foreign import ccall unsafe "SDL_JoystickName" sdlJoystickName :: JoystickIndex -> IO CString
tryName :: JoystickIndex -> IO (Maybe String)
tryName index = sdlJoystickName index >>= maybePeek peekCString

name :: JoystickIndex -> IO String
name = unwrapMaybe "SDL_JoystickName" . tryName

-- SDL_Joystick *SDL_JoystickOpen(int index);
foreign import ccall unsafe "SDL_JoystickOpen" sdlJoystickOpen :: JoystickIndex -> IO (Ptr JoystickStruct)
tryOpen :: JoystickIndex -> IO (Maybe Joystick)
tryOpen index = sdlJoystickOpen index >>= maybePeek mkFinalizedJoystick

open :: JoystickIndex -> IO Joystick
open = unwrapMaybe "SDL_JoystickOpen" . tryOpen

-- int SDL_JoystickOpened(int index);
foreign import ccall unsafe "SDL_JoystickOpened" sdlJoystickOpened :: JoystickIndex -> IO Int
opened :: JoystickIndex -> IO Bool
opened = fmap toBool . sdlJoystickOpened

-- int SDL_JoystickIndex(SDL_Joystick *joystick);
foreign import ccall unsafe "SDL_JoystickIndex" sdlJoystickIndex :: Ptr JoystickStruct -> JoystickIndex
index :: Joystick -> JoystickIndex
index joystick
    = unsafePerformIO $
      withForeignPtr joystick $
      return . sdlJoystickIndex

-- int SDL_JoystickNumAxes(SDL_Joystick *joystick);
foreign import ccall unsafe "SDL_JoystickNumAxes" sdlJoystickNumAxes :: Ptr JoystickStruct -> Int
axesAvailable :: Joystick -> Int
axesAvailable joystick
    = unsafePerformIO $
      withForeignPtr joystick $
      return . sdlJoystickNumAxes

-- int SDL_JoystickNumBalls(SDL_Joystick *joystick);
foreign import ccall unsafe "SDL_JoystickNumBalls" sdlJoystickNumBalls :: Ptr JoystickStruct -> Int
ballsAvailable :: Joystick -> Int
ballsAvailable joystick
    = unsafePerformIO $
      withForeignPtr joystick $
      return . sdlJoystickNumBalls

-- int SDL_JoystickNumHats(SDL_Joystick *joystick);
foreign import ccall unsafe "SDL_JoystickNumHats" sdlJoystickNumHats :: Ptr JoystickStruct -> Int
hatsAvailable :: Joystick -> Int
hatsAvailable joystick
    = unsafePerformIO $
      withForeignPtr joystick $
      return . sdlJoystickNumHats

-- int SDL_JoystickNumButtons(SDL_Joystick *joystick);
foreign import ccall unsafe "SDL_JoystickNumButtons" sdlJoystickNumButtons :: Ptr JoystickStruct -> Int
buttonsAvailable :: Joystick -> Int
buttonsAvailable joystick
    = unsafePerformIO $
      withForeignPtr joystick $
      return . sdlJoystickNumButtons

-- void SDL_JoystickUpdate(void);
foreign import ccall unsafe "SDL_JoystickUpdate" update :: IO ()

-- Sint16 SDL_JoystickGetAxis(SDL_Joystick *joystick, int axis);
foreign import ccall unsafe "SDL_JoystickGetAxis" joystickGetAxis :: Ptr JoystickStruct -> Int -> IO Int16
getAxis :: Joystick -> Word8 -> IO Int16
getAxis joystick axis
    = withForeignPtr joystick $ \ptr ->
      joystickGetAxis ptr (fromIntegral axis)

-- Uint8 SDL_JoystickGetHat(SDL_Joystick *joystick, int hat);
foreign import ccall unsafe "SDL_JoystickGetHat" joystickGetHat :: Ptr JoystickStruct -> Int -> IO Word8
getHat :: Joystick -> Word8 -> IO [Hat]
getHat joystick axis
    = withForeignPtr joystick $ \ptr ->
      fmap (fromBitmask.fromIntegral) (joystickGetHat ptr (fromIntegral axis))

-- Uint8 SDL_JoystickGetButton(SDL_Joystick *joystick, int button);
foreign import ccall unsafe "SDL_JoystickGetButton" joystickGetButton :: Ptr JoystickStruct -> Int -> IO Word8
getButton :: Joystick -> Word8 -> IO Bool
getButton joystick button
    = withForeignPtr joystick $ \ptr ->
      fmap toBool (joystickGetButton ptr (fromIntegral button))

-- int SDL_JoystickGetBall(SDL_Joystick *joystick, int ball, int *dx, int *dy);
foreign import ccall unsafe "SDL_JoystickGetBall" joystickGetBall
    :: Ptr JoystickStruct -> Int -> Ptr Int -> Ptr Int -> IO Int
getBall :: Joystick -> Word8 -> IO (Maybe (Int16,Int16))
getBall joystick ball
    = withForeignPtr joystick $ \ptr ->
      alloca $ \xrelPtr ->
      alloca $ \yrelPtr ->
      do ret <- joystickGetBall ptr (fromIntegral ball) xrelPtr yrelPtr
         case ret of
           0 -> do [xrel,yrel] <- mapM (fmap fromIntegral . peek) [xrelPtr,yrelPtr]
                   return $! Just (xrel,yrel)
           _ -> return Nothing

close :: Joystick -> IO ()
close =
#if defined(__GLASGOW_HASKELL__)
  finalizeForeignPtr
#else
  const (return ())
#endif

-- void SDL_JoystickClose(SDL_Joystick *joystick);
foreign import ccall unsafe "&SDL_JoystickClose" sdlCloseJoystickFinal :: FunPtr (Ptr JoystickStruct -> IO ())

mkFinalizedJoystick :: Ptr JoystickStruct -> IO Joystick
mkFinalizedJoystick = newForeignPtr sdlCloseJoystickFinal

