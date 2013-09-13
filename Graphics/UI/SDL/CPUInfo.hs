module Graphics.UI.SDL.CPUInfo
    ( getCPUCacheLineSize
    , getCPUCount
    , hasRDTSC
    , hasMMX
    , has3DNow
    , hasSSE
    , hasSSE2
    , hasSSE3
    , hasSSE41
    , hasSSE42
    , hasAltiVec
    ) where

import Foreign.C
import Foreign.Marshal.Utils (toBool)

import Graphics.UI.SDL.Types

-- int SDL_GetCPUCacheLineSize(void)
foreign import ccall unsafe "SDL_GetCPUCacheLineSize"
  sdlGetCPUCacheLineSize :: CInt

-- | Use this function to determine the L1 cache line size of the CPU.
getCPUCacheLineSize :: Int
getCPUCacheLineSize = fromIntegral sdlGetCPUCacheLineSize

-- int SDL_GetCPUCount(void)
foreign import ccall unsafe "SDL_GetCPUCount"
  sdlGetCPUCount :: CInt

-- | Use this function to return the number of CPU cores available.
getCPUCount :: Int
getCPUCount = fromIntegral sdlGetCPUCount



foreign import ccall unsafe "SDL_HasRDTSC" sdlHasRDTSC :: SDL_bool
-- |This function returns @True@ if the CPU has the RDTSC instruction.
hasRDTSC :: Bool
hasRDTSC = toBool sdlHasRDTSC

foreign import ccall unsafe "SDL_HasMMX" sdlHasMMX :: SDL_bool
-- |This function returns @True@ if the CPU has MMX features.
hasMMX :: Bool
hasMMX = toBool sdlHasMMX

foreign import ccall unsafe "SDL_Has3DNow" sdlHas3DNow :: SDL_bool
-- |This function returns @True@ if the CPU has 3DNow features.
has3DNow :: Bool
has3DNow = toBool sdlHas3DNow

foreign import ccall unsafe "SDL_HasSSE" sdlHasSSE :: SDL_bool
-- |This function returns @True@ if the CPU has SSE features.
hasSSE :: Bool
hasSSE = toBool sdlHasSSE

foreign import ccall unsafe "SDL_HasSSE2" sdlHasSSE2 :: SDL_bool
-- |This function returns @True@ if the CPU has SSE2 features.
hasSSE2 :: Bool
hasSSE2 = toBool sdlHasSSE2

foreign import ccall unsafe "SDL_HasSSE3" sdlHasSSE3 :: SDL_bool
-- |This function returns @True@ if the CPU has SSE3 features.
hasSSE3 :: Bool
hasSSE3 = toBool sdlHasSSE3

foreign import ccall unsafe "SDL_HasSSE41" sdlHasSSE41 :: SDL_bool
-- |This function returns @True@ if the CPU has SSE41 features.
hasSSE41 :: Bool
hasSSE41 = toBool sdlHasSSE41

foreign import ccall unsafe "SDL_HasSSE42" sdlHasSSE42 :: SDL_bool
-- |This function returns @True@ if the CPU has SSE42 features.
hasSSE42 :: Bool
hasSSE42 = toBool sdlHasSSE42


foreign import ccall unsafe "SDL_HasAltiVec" sdlHasAltiVec :: SDL_bool
-- |This function returns @True@ if the CPU has AltiVec features.
hasAltiVec :: Bool
hasAltiVec = toBool sdlHasAltiVec

