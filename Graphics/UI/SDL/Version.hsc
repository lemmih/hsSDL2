#include "SDL.h"
module Graphics.UI.SDL.Version
    ( compiledVersion
    , getRevision
    , getRevisionNumber
    , getVersion
    ) where

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

import Control.Applicative
import Data.Int
import Data.Version (Version(Version))
import Foreign (Word8, Ptr , Storable (..), alloca)
import Foreign.C.String

--------------------------------------------------------------------------------
data SDLVersion = SDLVersion #{type Uint8} #{type Uint8} #{type Uint8}
  deriving (Eq, Show)

instance Storable SDLVersion where
  sizeOf = const #{size SDL_version}

  alignment = const #{alignment SDL_version}

  peek ptr = SDLVersion
    <$> #{peek SDL_version, major} ptr
    <*> #{peek SDL_version, minor} ptr
    <*> #{peek SDL_version, patch} ptr

  poke ptr (SDLVersion major minor patch) = do
     #{poke SDL_version, major} ptr major
     #{poke SDL_version, minor} ptr minor
     #{poke SDL_version, patch} ptr patch

--------------------------------------------------------------------------------
-- | The SDL version your program was compiled against.
compiledVersion :: Version
compiledVersion = Version
    [ #{const SDL_MAJOR_VERSION}
    , #{const SDL_MINOR_VERSION}
    , #{const SDL_PATCHLEVEL}
    ] []

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_GetVersion"
  sdlGetVersion :: Ptr SDLVersion -> IO ()

-- | The version of SDL that is linked against your program.
getVersion :: IO Version
getVersion = alloca $ \versionPtr -> do
  sdlGetVersion versionPtr
  SDLVersion major minor patch <- peek versionPtr
  return (Version (map fromIntegral [major,minor,patch]) [])

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_GetRevision"
  sdlGetRevision :: IO CString

-- | The revision of SDL your program was linked against
--
-- This returns a hash uniquely identifying the exact revision of SDL in
-- use, not an incrementing number. This is only useful in comparing
-- against other revisions.
getRevision :: IO String
getRevision = sdlGetRevision >>= peekCString

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_GetRevisionNumber"
  sdlGetRevisionNumber :: IO #{type int}

-- | The revision number of SDL your program was linked against
--
-- This returns a number uniquely identifying the exact revision of SDL in
-- use. This is an incrementing number based on commits to hg.libsdl.org.
getRevisionNumber :: IO #{type int}
getRevisionNumber = sdlGetRevisionNumber
