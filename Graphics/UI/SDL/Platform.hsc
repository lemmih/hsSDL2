#include "SDL.h"
module Graphics.UI.SDL.Platform
  ( getPlatform
  , Platform(..)
  ) where

import Foreign.C (CString, peekCString)

foreign import ccall unsafe "SDL_GetPlatform"
  sdlGetPlatform :: IO CString

data Platform
  = AIX
  | Android
  | BSDI
  | Dreamcast
  | FreeBSD
  | Haiku
  | HPUX
  | Irix
  | Linux
  | AtariMint
  | MacOSClassic
  | MacOSX
  | NetBSD
  | OpenBSD
  | OS2
  | OSF1
  | QNXNeutrino
  | RISCOS
  | Solaris
  | Windows
  | IOS
  | PlayStationPortable
  | Unknown
  deriving (Eq, Show)

getPlatform :: IO Platform
getPlatform = sdlGetPlatform >>= peekCString >>= return . toPlatform
  where toPlatform s =
          case s of
            "AIX"           -> AIX
            "Android"       -> Android
            "BSDI"          -> BSDI
            "Dreamcast"     -> Dreamcast
            "FreeBSD"       -> FreeBSD
            "Haiku"         -> Haiku
            "HP-UX"         -> HPUX
            "Irix"          -> Irix
            "Linux"         -> Linux
            "Atari MiNT"    -> AtariMint
            "MacOS Classic" -> MacOSClassic
            "Mac OS X"      -> MacOSX
            "NetBSD"        -> NetBSD
            "OpenBSD"       -> OpenBSD
            "OS/2"          -> OS2
            "OSF/1"         -> OSF1
            "QNX Neutrino"  -> QNXNeutrino
            "RISC OS"       -> RISCOS
            "Solaris"       -> Solaris
            "Windows"       -> Windows
            "iOS"           -> IOS
            "PlayStation Portable"         -> PlayStationPortable
            "Unknown (see SDL_platform.h)" -> Unknown
            _ -> error $ "getPlatform: unhandled platform type " ++ s

