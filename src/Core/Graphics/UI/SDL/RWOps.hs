-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.SDL.Video
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Graphics.UI.SDL.RWOps
    ( fromFile
    , tryFromFile
    , free
    , with
    , mkFinalizedRW
    ) where

import Foreign (Ptr, FunPtr,
#if defined(__GLASGOW_HASKELL__)
  finalizeForeignPtr,
#endif
  maybePeek, newForeignPtr)
import Foreign.C (withCString, CString)

import Control.Exception (bracket)

import Graphics.UI.SDL.Types (RWops, RWopsStruct)
import Graphics.UI.SDL.General (unwrapMaybe)


with :: FilePath -> String -> (RWops -> IO a) -> IO a
with path mode action
    = bracket (fromFile path mode)
              (free)
              action


-- extern DECLSPEC SDL_RWops * SDLCALL SDL_RWFromFile(const char *file, const char *mode);
foreign import ccall unsafe "SDL_RWFromFile" rwFromFile :: CString -> CString -> IO (Ptr RWopsStruct)
tryFromFile :: FilePath -> String -> IO (Maybe RWops)
tryFromFile filepath mode
    = withCString filepath $ \cPath ->
      withCString mode $ \cMode ->
      rwFromFile cPath cMode >>= maybePeek mkFinalizedRW

fromFile :: FilePath -> String -> IO RWops
fromFile filepath mode = unwrapMaybe "SDL_RWFromFile" (tryFromFile filepath mode)

-- extern DECLSPEC void SDLCALL SDL_FreeRW(SDL_RWops *area);
foreign import ccall unsafe "&SDL_FreeRW" rwFreeFinal :: FunPtr (Ptr RWopsStruct -> IO ())
mkFinalizedRW :: Ptr RWopsStruct -> IO RWops
mkFinalizedRW = newForeignPtr rwFreeFinal

free :: RWops -> IO ()
free =
#if defined(__GLASGOW_HASKELL__)
  finalizeForeignPtr
#else
  const (return ())
#endif

{-
foreign import ccall unsafe "SDL_FreeRW" rwFree :: Ptr RWopsStruct -> IO ()
free :: RWops -> IO ()
free rw = withForeignPtr rw rwFree 
-}

