#include "SDL.h"
module Graphics.UI.SDL.Init
    ( -- * Haskell Wrappers
      withInit

      -- * SDL Routines
    , InitFlag(..)
    , init
    , initSubSystem
    , quit
    , quitSubSystem
    , wasInit
    ) where

import Prelude hiding (init)

import Control.Applicative
import Control.Exception (bracket_)
import Data.Int
import Data.Word
import Graphics.UI.SDL.Utilities (fatalSDLBool, toBitmask, fromBitmask)

--------------------------------------------------------------------------------
withInit :: [InitFlag] -> IO a -> IO a
withInit flags = bracket_ (init flags) quit

--------------------------------------------------------------------------------
data InitFlag
   = InitTimer
   | InitAudio
   | InitVideo
   | InitJoystick
   | InitHaptic
   | InitGameController
   | InitEvents
   | InitEverything
   | InitNoParachute
   deriving (Eq, Ord, Show, Read, Bounded, Enum)

initFlagToC :: InitFlag -> #{type Uint32}
initFlagToC InitTimer          = #{const SDL_INIT_TIMER}
initFlagToC InitAudio          = #{const SDL_INIT_AUDIO}
initFlagToC InitVideo          = #{const SDL_INIT_VIDEO}
initFlagToC InitJoystick       = #{const SDL_INIT_JOYSTICK}
initFlagToC InitHaptic         = #{const SDL_INIT_HAPTIC}
initFlagToC InitGameController = #{const SDL_INIT_GAMECONTROLLER}
initFlagToC InitEvents         = #{const SDL_INIT_EVENTS}
initFlagToC InitEverything     = #{const SDL_INIT_EVERYTHING}
initFlagToC InitNoParachute    = #{const SDL_INIT_NOPARACHUTE}

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_Init"
  sdlInit :: #{type Uint32} -> IO #{type int}

init :: [InitFlag] -> IO ()
init = fatalSDLBool "SDL_Init" .
  sdlInit . toBitmask initFlagToC

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_Quit"
  quit :: IO ()

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_InitSubSystem"
  sdlInitSubSystem :: #{type Uint32} -> IO #{type int}

initSubSystem :: [InitFlag] -> IO ()
initSubSystem = fatalSDLBool "SDL_InitSubSystem" .
  sdlInitSubSystem . toBitmask initFlagToC

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_QuitSubSystem"
  sdlQuitSubSystem :: #{type Uint32} -> IO ()

quitSubSystem :: [InitFlag] -> IO ()
quitSubSystem = sdlQuitSubSystem . toBitmask initFlagToC

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_WasInit"
  sdlWasInit :: #{type Uint32} -> IO #{type Uint32}

wasInit :: [InitFlag] -> IO [InitFlag]
wasInit flags = 
  fromBitmask initFlagToC <$> sdlWasInit (toBitmask initFlagToC flags)
