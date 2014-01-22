#include "SDL.h"
module Graphics.UI.SDL.Timer
    ( addTimer
    , removeTimer
    , delay
    , getPerformanceCounter
    , getPerformanceFrequency
    , getTicks
    , ticksPassed
    ) where

import Foreign
import Control.Applicative ((<$>))

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
  TimerCallback <$> sdlAddTimer interval cb nullPtr

foreign import ccall "SDL_RemoveTimer"
  sdlRemoveTimer :: #{type SDL_TimerID} -> IO #{type SDL_bool}

removeTimer :: TimerCallback -> IO Bool
removeTimer (TimerCallback tId) = do
  (== #{const SDL_TRUE}) <$> sdlRemoveTimer tId

foreign import ccall unsafe "SDL_Delay"
  sdlDelay :: Word32 -> IO ()

delay :: Word32 -> IO ()
delay = sdlDelay

foreign import ccall unsafe "SDL_GetPerformanceCounter"
  sdlGetPerformanceCounter :: IO (#{type Uint64})

getPerformanceCounter :: IO (#{type Uint64})
getPerformanceCounter = sdlGetPerformanceCounter

foreign import ccall unsafe "SDL_GetPerformanceFrequency"
  sdlGetPerformanceFrequency :: IO (#{type Uint64})

getPerformanceFrequency :: IO (#{type Uint64})
getPerformanceFrequency = sdlGetPerformanceFrequency

foreign import ccall unsafe "SDL_GetTicks"
  sdlGetTicks :: IO (#{type Uint32})

getTicks :: IO (#{type Uint32})
getTicks = sdlGetTicks

ticksPassed :: #{type Uint32} -> #{type Uint32} -> Bool
ticksPassed a b = a - b <= 0

