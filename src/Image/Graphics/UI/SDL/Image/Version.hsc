#include "SDL_image.h"
module Graphics.UI.SDL.Image.Version
    ( compiledFor
    , linkedWith
    ) where

import Data.Version (Version(Version))

import Foreign (Word8, Ptr, Storable(sizeOf, alignment, peekByteOff, peek))

data SDLVersion
    = SDLVersion Word8 Word8 Word8

instance Storable SDLVersion where
    sizeOf _ = #{size SDL_version}
    alignment _ = 1
    peek ptr = do major <- #{peek SDL_version, major} ptr
                  minor <- #{peek SDL_version, minor} ptr
                  patch <- #{peek SDL_version, patch} ptr
                  return (SDLVersion major minor patch)

compiledFor :: Version
compiledFor = Version [ #{const SDL_IMAGE_MAJOR_VERSION}
                      , #{const SDL_IMAGE_MINOR_VERSION}
                      , #{const SDL_IMAGE_PATCHLEVEL}
                      ] []

foreign import ccall unsafe "IMG_Linked_Version" sdlLinkedVersion :: IO (Ptr SDLVersion)
linkedWith :: IO Version
linkedWith = do versionPtr <- sdlLinkedVersion
                SDLVersion major minor patch <- peek versionPtr
                return (Version (map fromIntegral [major,minor,patch]) [])
