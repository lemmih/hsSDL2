-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.SDL.General
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Graphics.UI.SDL.General
    ( init
    , withInit
    , initSubSystem
    , quitSubSystem
    , quit
    , wasInit
    , getError
    , failWithError
    , unwrapBool
    , unwrapMaybe
    , unwrapInt
    , InitFlag(..)
    ) where

import Foreign ()
import Foreign.C (peekCString,CString)
import Data.Maybe (fromMaybe)
import Control.Monad (when)
import Data.Word (Word32)
import Data.Int ()

import Control.Exception

import Prelude hiding (init)

import Graphics.UI.SDL.Utilities

{-
  Auto generated Enum info.
-}
data InitFlag = InitTimer
              | InitAudio
              | InitVideo
              | InitCDROM
              | InitJoystick
              | InitNoParachute
              | InitEventthread
              | InitEverything
    deriving (Eq, Ord, Show)
instance Bounded InitFlag where
      minBound = InitTimer
      maxBound = InitEventthread
instance Enum InitFlag where
      fromEnum InitTimer = 1
      fromEnum InitAudio = 16
      fromEnum InitVideo = 32
      fromEnum InitCDROM = 256
      fromEnum InitJoystick = 512
      fromEnum InitNoParachute = 1048576
      fromEnum InitEventthread = 16777216
      fromEnum InitEverything = 65535
      toEnum 1 = InitTimer
      toEnum 16 = InitAudio
      toEnum 32 = InitVideo
      toEnum 256 = InitCDROM
      toEnum 512 = InitJoystick
      toEnum 1048576 = InitNoParachute
      toEnum 16777216 = InitEventthread
      toEnum 65535 = InitEverything
      succ InitTimer = InitAudio
      succ InitAudio = InitVideo
      succ InitVideo = InitCDROM
      succ InitCDROM = InitJoystick
      succ InitJoystick = InitNoParachute
      succ InitNoParachute = InitEventthread
      succ InitEventthread = InitEverything
      pred InitAudio = InitTimer
      pred InitVideo = InitAudio
      pred InitCDROM = InitVideo
      pred InitJoystick = InitCDROM
      pred InitNoParachute = InitJoystick
      pred InitEventthread = InitNoParachute
      pred InitEverything = InitEventthread
      enumFromTo x y | x > y = []
                     | x == y = [y]
                     | True = x : enumFromTo (succ x) y

unwrapMaybe :: String -> IO (Maybe a) -> IO a
unwrapMaybe errMsg action
    = do val <- action
         case val of
           Just a -> return a
           Nothing -> failWithError errMsg

unwrapInt :: (Int -> Bool) -> String -> IO Int -> IO Int
unwrapInt fn errMsg action
    = do val <- action
         if fn val
            then return val
            else failWithError errMsg

unwrapBool :: String -> IO Bool -> IO ()
unwrapBool errMsg action
    = do val <- action
         case val of
           True -> return ()
           False -> failWithError errMsg

foreign import ccall unsafe "SDL_Init" sdlInit :: Word32 -> IO Int
-- | Initializes SDL. This should be called before all other SDL functions.
init :: [InitFlag] -> IO ()
init flags
    = do ret <- sdlInit (fromIntegral (toBitmask flags))
         when (ret == (-1)) (failWithError "SDL_Init")

withInit :: [InitFlag] -> IO a -> IO a
withInit flags action
    = bracket_ (init flags) quit action

foreign import ccall unsafe "SDL_InitSubSystem" sdlInitSubSystem :: Word32 -> IO Int
-- | After SDL has been initialized with SDL_Init you may initialize
-- uninitialized subsystems with SDL_InitSubSystem.
initSubSystem :: [InitFlag] -> IO ()
initSubSystem flags
    = do ret <- sdlInitSubSystem (fromIntegral (toBitmask flags))
         when (ret == (-1)) (failWithError "SDL_InitSubSystem")

foreign import ccall unsafe "SDL_QuitSubSystem" sdlQuitSubSystem :: Word32 -> IO ()
quitSubSystem :: [InitFlag] -> IO ()
quitSubSystem = sdlQuitSubSystem . fromIntegral . toBitmask

foreign import ccall unsafe "SDL_Quit" sdlQuit :: IO ()
quit :: IO ()
quit = sdlQuit

foreign import ccall unsafe "SDL_WasInit" sdlWasInit :: Word32 -> IO Word32
-- | wasInit allows you to see which SDL subsytems have been initialized
wasInit :: [InitFlag] -> IO [InitFlag]
wasInit flags
    = do ret <- sdlWasInit (fromIntegral (toBitmask flags))
         return (fromBitmask (fromIntegral ret))


foreign import ccall unsafe "SDL_GetError" sdlGetError :: IO CString
-- | Returns a string containing the last error. Nothing if no error.
getError :: IO (Maybe String)
getError
    = do str <- peekCString =<< sdlGetError 
         if null str
            then return Nothing
            else return (Just str)

failWithError :: String -> IO a
failWithError msg
    = do err <- fmap (fromMaybe "No SDL error") getError
         ioError $ userError $ msg ++ "\nSDL message: " ++ err

