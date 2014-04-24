#include "SDL.h"
module Graphics.UI.SDL.Keyboard
  ( Keymod(..)
  , getKeyboardFocus
  , getModState
  , setModState
  ) where

import Foreign
import Graphics.UI.SDL.Types
import Graphics.UI.SDL.Raw

data Keymod
   = KeymodNone
   | KeymodLShift
   | KeymodRShift
   | KeymodLCtrl
   | KeymodRCtrl
   | KeymodLAlt
   | KeymodRAlt
   | KeymodLGui
   | KeymodRGui
   | KeymodNum
   | KeymodCaps
   | KeymodMode
   | KeymodReserved
   deriving (Eq, Show)

fromKeymod :: Keymod -> #{type SDL_Keymod}
fromKeymod KeymodNone = #{const KMOD_NONE}
fromKeymod KeymodLShift = #{const KMOD_LSHIFT}
fromKeymod KeymodRShift = #{const KMOD_RSHIFT}
fromKeymod KeymodLCtrl = #{const KMOD_LCTRL}
fromKeymod KeymodRCtrl = #{const KMOD_RCTRL}
fromKeymod KeymodLAlt = #{const KMOD_LALT}
fromKeymod KeymodRAlt = #{const KMOD_RALT}
fromKeymod KeymodLGui = #{const KMOD_LGUI}
fromKeymod KeymodRGui = #{const KMOD_RGUI}
fromKeymod KeymodNum = #{const KMOD_NUM}
fromKeymod KeymodCaps = #{const KMOD_CAPS}
fromKeymod KeymodMode = #{const KMOD_MODE}
fromKeymod KeymodReserved = #{const KMOD_RESERVED}

toKeymod :: #{type SDL_Keymod} -> Keymod
toKeymod #{const KMOD_NONE} = KeymodNone
toKeymod #{const KMOD_LSHIFT} = KeymodLShift
toKeymod #{const KMOD_RSHIFT} = KeymodRShift
toKeymod #{const KMOD_LCTRL} = KeymodLCtrl
toKeymod #{const KMOD_RCTRL} = KeymodRCtrl
toKeymod #{const KMOD_LALT} = KeymodLAlt
toKeymod #{const KMOD_RALT} = KeymodRAlt
toKeymod #{const KMOD_LGUI} = KeymodLGui
toKeymod #{const KMOD_RGUI} = KeymodRGui
toKeymod #{const KMOD_NUM} = KeymodNum
toKeymod #{const KMOD_CAPS} = KeymodCaps
toKeymod #{const KMOD_MODE} = KeymodMode
toKeymod #{const KMOD_RESERVED} = KeymodReserved
toKeymod _ = error "unhandled keymod"

foreign import ccall unsafe "SDL_GetKeyboardFocus"
  sdlGetKeyboardFocus :: IO (Ptr WindowStruct)

getKeyboardFocus :: IO Window
getKeyboardFocus =
  sdlGetKeyboardFocus >>= mkFinalizedWindow

foreign import ccall unsafe "SDL_GetModState"
  sdlGetModState :: IO #{type SDL_Keymod}

getModState :: IO Keymod
getModState = sdlGetModState >>= return . toKeymod

foreign import ccall unsafe "SDL_SetModState"
  sdlSetModState :: #{type SDL_Keymod} -> IO ()

setModState :: Keymod -> IO ()
setModState keymod = sdlSetModState $ fromKeymod keymod

foreign import ccall unsafe "SDL_GetKeyFromScancode"
  sdlGetKeyFromScancode :: ScanCode -> IO Keysym

