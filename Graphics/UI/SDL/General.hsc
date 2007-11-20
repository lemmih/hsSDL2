#include "SDL.h"
#ifdef main
#undef main
#endif
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

import Foreign.C (peekCString,CString)
import Data.Maybe (fromMaybe)
import Control.Monad (when)
import Data.Word (Word32)

import Control.Exception (bracket_)

import Prelude hiding (init,Enum(..))

import Graphics.UI.SDL.Utilities (Enum(..), toBitmask, fromBitmask)


data InitFlag = InitTimer
              | InitAudio
              | InitVideo
              | InitCDROM
              | InitJoystick
              | InitNoParachute
              | InitEventthread
              | InitEverything
    deriving (Eq, Ord, Show, Read)
instance Bounded InitFlag where
      minBound = InitTimer
      maxBound = InitEventthread

instance Enum InitFlag Word32 where
      fromEnum InitTimer = #{const SDL_INIT_TIMER}
      fromEnum InitAudio = #{const SDL_INIT_AUDIO}
      fromEnum InitVideo = #{const SDL_INIT_VIDEO}
      fromEnum InitCDROM = #{const SDL_INIT_CDROM}
      fromEnum InitJoystick = #{const SDL_INIT_JOYSTICK}
      fromEnum InitNoParachute = #{const SDL_INIT_NOPARACHUTE}
      fromEnum InitEventthread = #{const SDL_INIT_EVENTTHREAD}
      fromEnum InitEverything = #{const SDL_INIT_EVERYTHING}
      toEnum #{const SDL_INIT_TIMER} = InitTimer
      toEnum #{const SDL_INIT_AUDIO} = InitAudio
      toEnum #{const SDL_INIT_VIDEO}= InitVideo
      toEnum #{const SDL_INIT_CDROM} = InitCDROM
      toEnum #{const SDL_INIT_JOYSTICK} = InitJoystick
      toEnum #{const SDL_INIT_NOPARACHUTE} = InitNoParachute
      toEnum #{const SDL_INIT_EVENTTHREAD} = InitEventthread
      toEnum #{const SDL_INIT_EVERYTHING} = InitEverything
      toEnum _ = error "Graphics.UI.SDL.General.toEnum: bad argument"
      succ InitTimer = InitAudio
      succ InitAudio = InitVideo
      succ InitVideo = InitCDROM
      succ InitCDROM = InitJoystick
      succ InitJoystick = InitNoParachute
      succ InitNoParachute = InitEventthread
      succ InitEventthread = InitEverything
      succ _ = error "Graphics.UI.SDL.General.succ: bad argument"
      pred InitAudio = InitTimer
      pred InitVideo = InitAudio
      pred InitCDROM = InitVideo
      pred InitJoystick = InitCDROM
      pred InitNoParachute = InitJoystick
      pred InitEventthread = InitNoParachute
      pred InitEverything = InitEventthread
      pred _ = error "Graphics.UI.SDL.General.pred: bad argument"
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

