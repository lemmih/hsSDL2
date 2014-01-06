module Graphics.UI.SDL.Initl
    ( init
    ) where

import Graphics.UI.SDL.Utilities (toBitmask, fromBitmask)

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
init = fatalSDLBool "SDL_Init" . sdlInit . bitmask . map initFlagToC
