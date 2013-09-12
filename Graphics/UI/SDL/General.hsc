#include "SDL.h"
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
    ( -- * Initialization
      withInit
    , InitFlag(..)
    , init
    , initSubSystem
    , quitSubSystem
    , quit
    , wasInit

      -- * Error Handling
    , getError

      -- * Hints
    , clearHints
    , getHint
    , setHint
    , setHintWithPriority
    , HintPriority(..)

      -- * Utilities
    , failWithError
    , unwrapBool
    , unwrapMaybe
    , unwrapInt
    ) where

import Prelude hiding (init,Enum(..))
import Control.Exception (bracket_)
import Control.Monad ((>=>), when)
import Data.Maybe (fromMaybe)
import Data.Word (Word32)
import Foreign.C (peekCString,CString,withCString)

import Graphics.UI.SDL.Utilities (Enum(..), toBitmask, fromBitmask)


data InitFlag = InitTimer
              | InitAudio
              | InitVideo
              | InitJoystick
              | InitHaptic
              | InitGameController
              | InitEvents
              | InitEverything
              | InitNoParachute
    deriving (Eq, Ord, Show, Read)
instance Bounded InitFlag where
      minBound = InitTimer
      maxBound = InitNoParachute

instance Enum InitFlag Word32 where
      fromEnum InitTimer          = #{const SDL_INIT_TIMER}
      fromEnum InitAudio          = #{const SDL_INIT_AUDIO}
      fromEnum InitVideo          = #{const SDL_INIT_VIDEO}
      fromEnum InitJoystick       = #{const SDL_INIT_JOYSTICK}
      fromEnum InitHaptic         = #{const SDL_INIT_HAPTIC}
      fromEnum InitGameController = #{const SDL_INIT_GAMECONTROLLER}
      fromEnum InitEvents         = #{const SDL_INIT_EVENTS}
      fromEnum InitEverything     = #{const SDL_INIT_EVERYTHING}
      fromEnum InitNoParachute    = #{const SDL_INIT_NOPARACHUTE}
      toEnum #{const SDL_INIT_TIMER}       = InitTimer
      toEnum #{const SDL_INIT_AUDIO}       = InitAudio
      toEnum #{const SDL_INIT_VIDEO}       = InitVideo
      toEnum #{const SDL_INIT_JOYSTICK}    = InitJoystick
      toEnum #{const SDL_INIT_HAPTIC}      = InitHaptic
      toEnum #{const SDL_INIT_EVERYTHING}  = InitEverything
      toEnum #{const SDL_INIT_NOPARACHUTE} = InitNoParachute
      toEnum _ = error "Graphics.UI.SDL.General.toEnum: bad argument"
      -- FIXME: succ is incomplete.
      succ InitTimer = InitAudio
      succ InitAudio = InitVideo
      succ InitVideo = InitJoystick
      succ InitJoystick = InitHaptic
      succ InitHaptic = InitGameController
      succ _ = error "Graphics.UI.SDL.General.succ: bad argument"
      -- FIXME: pred is incomplete.
      pred InitAudio = InitTimer
      pred InitVideo = InitAudio
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
getError = sdlGetError >>= maybeString

failWithError :: String -> IO a
failWithError msg
    = do err <- fmap (fromMaybe "No SDL error") getError
         ioError $ userError $ msg ++ "\nSDL message: " ++ err

foreign import ccall unsafe "SDL_ClearHints" sdlClearHints :: IO ()

-- | Clear all hints.
clearHints :: IO ()
clearHints = sdlClearHints

foreign import ccall unsafe "SDL_GetHint" sdlGetHint :: CString -> IO CString
-- | Returns 'Just' the string value of a hint or 'Nothing' if the hint is not
-- set.
getHint :: String -> IO (Maybe String)
getHint hint = withCString hint (sdlGetHint >=> maybeString)

foreign import ccall unsafe "SDL_SetHint" sdlSetHint :: CString -> CString -> IO Bool
-- | Set a hint with normal priority. Returns 'True' if the hint was set,
-- 'False' otherwise.
setHint :: String -> String -> IO Bool
setHint k v =
  withCString k $ \cK -> 
  withCString v $ \cV ->
  sdlSetHint cK cV

data HintPriority = HintDefault | HintNormal | HintOverride

foreign import ccall unsafe "SDL_SetHintWithPriority" sdlSetHintWithPriority
  :: CString -> CString -> Int -> IO Bool

-- | Set a hint with normal priority. Returns 'True' if the hint was set,
-- 'False' otherwise.
setHintWithPriority :: String -> String -> HintPriority -> IO Bool
setHintWithPriority k v priority =
  withCString k $ \cK -> 
  withCString v $ \cV ->
  sdlSetHintWithPriority cK cV $
    case priority of
      HintDefault -> #{const SDL_HINT_DEFAULT}
      HintNormal -> #{const SDL_HINT_NORMAL}
      HintOverride -> #{const SDL_HINT_OVERRIDE}

maybeString :: CString -> IO (Maybe String)
maybeString = fmap maybeString . peekCString
  where maybeString str | null str = Nothing
                        | otherwise = Just str

