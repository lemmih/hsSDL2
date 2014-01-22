#include "SDL.h"
module Graphics.UI.SDL.Timer
    ( addTimer
    , removeTimer
    , delay
    , getPerformanceCounter
    , getPerformanceFrequency
    , getTicks
    , ticksPassed
    , TimerCallback
    ) where

import Foreign
import Control.Applicative ((<$>))

import Graphics.UI.SDL.General (failWithError)
import Graphics.UI.SDL.Utilities (sdlBoolToBool)

foreign import ccall "wrapper"
  mkTimerCallback :: (Word32 -> Ptr () -> IO Word32) -> IO (FunPtr (Word32 -> Ptr () -> IO Word32))

newtype TimerCallback = TimerCallback #{type SDL_TimerID}

foreign import ccall "SDL_AddTimer"
  sdlAddTimer :: Word32 -> FunPtr (Word32 -> Ptr () -> IO Word32) -> Ptr () -> IO #{type SDL_TimerID}

data TimerTermination = CancelTimer | ContinueTimer

addTimer :: Word32 -> (Word32 -> IO TimerTermination) -> IO TimerCallback
addTimer interval callback = do
  cb <- mkTimerCallback $ \passed _ -> do
    termination <- callback passed
    case termination of
      CancelTimer -> return 0
      ContinueTimer -> return 1
  tId <- sdlAddTimer interval cb nullPtr
  case tId of
    0 -> failWithError "addTimer"
    _ -> return $ TimerCallback tId

foreign import ccall "SDL_RemoveTimer"
  sdlRemoveTimer :: #{type SDL_TimerID} -> IO #{type SDL_bool}

removeTimer :: TimerCallback -> IO Bool
removeTimer (TimerCallback tId) = do
  sdlBoolToBool <$> sdlRemoveTimer tId

foreign import ccall unsafe "SDL_Delay"
  delay :: Word32 -> IO ()

foreign import ccall unsafe "SDL_GetPerformanceCounter"
  getPerformanceCounter :: IO (#{type Uint64})

foreign import ccall unsafe "SDL_GetPerformanceFrequency"
  getPerformanceFrequency :: IO (#{type Uint64})

foreign import ccall unsafe "SDL_GetTicks"
  getTicks :: IO (#{type Uint32})

ticksPassed :: #{type Uint32} -> #{type Uint32} -> Bool
ticksPassed a b = a - b <= 0

