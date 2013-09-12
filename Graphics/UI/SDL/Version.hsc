#include "SDL.h"
module Graphics.UI.SDL.Version
    ( compiledVersion
    , linkedVersion
    ) where

import Data.Version (Version(Version))

import Foreign (Word8, Ptr, Storable(sizeOf, alignment, peekByteOff, peek),alloca)

data SDLVersion
    = SDLVersion Word8 Word8 Word8

instance Storable SDLVersion where
    sizeOf _ = #{size SDL_version}
    alignment _ = 1
    peek ptr = do major <- #{peek SDL_version, major} ptr
                  minor <- #{peek SDL_version, minor} ptr
                  patch <- #{peek SDL_version, patch} ptr
                  return (SDLVersion major minor patch)

-- | Use this function to determine the SDL version your program was compiled against.
compiledVersion :: Version
compiledVersion = Version
    [ #{const SDL_MAJOR_VERSION}
    , #{const SDL_MINOR_VERSION}
    , #{const SDL_PATCHLEVEL}
    ] []

-- TODO
-- const char* SDL_GetRevision(void)

-- void SDL_GetVersion(SDL_version* ver)
foreign import ccall unsafe "SDL_GetVersion" sdlGetVersion :: Ptr SDLVersion -> IO ()
-- | Use this function to get the version of SDL that is linked against your program.
linkedVersion :: IO Version
linkedVersion = alloca $ \versionPtr -> do
  sdlGetVersion versionPtr
  SDLVersion major minor patch <- peek versionPtr
  return (Version (map fromIntegral [major,minor,patch]) [])

