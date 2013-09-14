module Graphics.UI.SDL.Version
    ( compiledVersion
    , linkedVersion
    , getRevision
    , getRevisionNumber
    ) where

#include "SDL.h"
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

import Control.Applicative
import Data.Version (Version(Version))

import Foreign (
    Word8,
    Ptr,
    Storable(..), --(sizeOf, alignment, peekByteOff, peek),
    alloca
    )
import Foreign.C.String
import Foreign.C.Types


data SDLVersion
    = SDLVersion Word8 Word8 Word8

instance Storable SDLVersion where
    sizeOf _ = #{size SDL_version}
    alignment _ = #{alignment SDL_version}
    peek ptr = SDLVersion
        <$> #{peek SDL_version, major} ptr
        <*> #{peek SDL_version, minor} ptr
        <*> #{peek SDL_version, patch} ptr
    poke = error "Sorry, you can't poke an SDLVersion"

-- | The SDL version your program was compiled against.
compiledVersion :: Version
compiledVersion = Version
    [ #{const SDL_MAJOR_VERSION}
    , #{const SDL_MINOR_VERSION}
    , #{const SDL_PATCHLEVEL}
    ] []

-- void SDL_GetVersion(SDL_version* ver)
foreign import ccall unsafe "SDL_GetVersion"
    sdlGetVersion :: Ptr SDLVersion -> IO ()

-- | The version of SDL that is linked against your program.
linkedVersion :: IO Version
linkedVersion = alloca $ \versionPtr -> do
  sdlGetVersion versionPtr
  SDLVersion major minor patch <- peek versionPtr
  return (Version (map fromIntegral [major,minor,patch]) [])

-- const char* SDL_GetRevision(void)
foreign import ccall unsafe "SDL_GetRevision"
    sdlGetRevision :: IO (Ptr CChar)

-- | The revision of SDL your program was linked against
--
-- This returns a hash uniquely identifying the exact revision of SDL in
-- use, not an incrementing number. This is only useful in comparing
-- against other revisions.
getRevision :: IO String
getRevision = sdlGetRevision >>= peekCString

foreign import ccall unsafe "SDL_GetRevisionNumber"
    sdlGetRevisionNumber :: IO CInt

-- | The revision number of SDL your program was linked against
--
-- This returns a number uniquely identifying the exact revision of SDL in
-- use. This is an incrementing number based on commits to hg.libsdl.org.
getRevisionNumber :: IO Int
getRevisionNumber = fromIntegral <$> sdlGetRevisionNumber
