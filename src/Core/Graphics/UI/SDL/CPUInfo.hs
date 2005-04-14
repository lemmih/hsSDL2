module Graphics.UI.SDL.CPUInfo
    ( hasRDTSC
    , hasMMX
    , hasMMXExt
    , has3DNow
    , has3DNowExt
    , hasSSE
    , hasSSE2
    , hasAltiVec
    ) where

import Foreign.Marshal.Utils (toBool)

foreign import ccall unsafe "SDL_HasRDTSC" sdlHasRDTSC :: Int
-- |This function returns @True@ if the CPU has the RDTSC instruction.
hasRDTSC :: Bool
hasRDTSC = toBool sdlHasRDTSC

foreign import ccall unsafe "SDL_HasMMX" sdlHasMMX :: Int
-- |This function returns @True@ if the CPU has MMX features.
hasMMX :: Bool
hasMMX = toBool sdlHasMMX

foreign import ccall unsafe "SDL_HasMMXExt" sdlHasMMXExt :: Int
-- |This function returns @True@ if the CPU has MMX Ext. features.
hasMMXExt :: Bool
hasMMXExt = toBool sdlHasMMXExt

foreign import ccall unsafe "SDL_Has3DNow" sdlHas3DNow :: Int
-- |This function returns @True@ if the CPU has 3DNow features.
has3DNow :: Bool
has3DNow = toBool sdlHas3DNow

foreign import ccall unsafe "SDL_Has3DNowExt" sdlHas3DNowExt :: Int
-- |This function returns @True@ if the CPU has 3DNow! Ext. features.
has3DNowExt :: Bool
has3DNowExt = toBool sdlHas3DNowExt

foreign import ccall unsafe "SDL_HasSSE" sdlHasSSE :: Int
-- |This function returns @True@ if the CPU has SSE features.
hasSSE :: Bool
hasSSE = toBool sdlHasSSE

foreign import ccall unsafe "SDL_HasSSE2" sdlHasSSE2 :: Int
-- |This function returns @True@ if the CPU has SSE2 features.
hasSSE2 :: Bool
hasSSE2 = toBool sdlHasSSE2

foreign import ccall unsafe "SDL_HasAltiVec" sdlHasAltiVec :: Int
-- |This function returns @True@ if the CPU has AltiVec features.
hasAltiVec :: Bool
hasAltiVec = toBool sdlHasAltiVec

