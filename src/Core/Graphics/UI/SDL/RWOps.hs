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
    , finalize
    , free
    , with
    , mkFinalizedRW
    ) where

import Foreign hiding (free,with)
import Foreign.C

import Control.Exception

import Graphics.UI.SDL.Types
import Graphics.UI.SDL.General


with :: FilePath -> String -> (RWops -> IO a) -> IO a
with path mode action
    = bracket (fromFile path mode)
              (finalize)
              action


-- extern DECLSPEC SDL_RWops * SDLCALL SDL_RWFromFile(const char *file, const char *mode);
foreign import ccall unsafe "SDL_RWFromFile" rwFromFile :: CString -> CString -> IO (Ptr RWopsStruct)
tryFromFile :: FilePath -> String -> IO (Maybe RWops)
tryFromFile filepath mode
    = withCString filepath $ \cPath ->
      withCString mode $ \cMode ->
      do rw <- rwFromFile cPath cMode
         if rw == nullPtr
            then return Nothing
            else fmap Just (mkFinalizedRW rw)

fromFile :: FilePath -> String -> IO RWops
fromFile filepath mode = unwrapMaybe "SDL_RWFromFile" (tryFromFile filepath mode)

-- extern DECLSPEC void SDLCALL SDL_FreeRW(SDL_RWops *area);
foreign import ccall unsafe "&SDL_FreeRW" rwFreeFinal :: FunPtr (Ptr RWopsStruct -> IO ())
mkFinalizedRW :: Ptr RWopsStruct -> IO RWops
mkFinalizedRW = newForeignPtr rwFreeFinal

finalize :: RWops -> IO ()
finalize = finalizeForeignPtr

foreign import ccall unsafe "SDL_FreeRW" rwFree :: Ptr RWopsStruct -> IO ()
free :: RWops -> IO ()
free rw = withForeignPtr rw rwFree 


