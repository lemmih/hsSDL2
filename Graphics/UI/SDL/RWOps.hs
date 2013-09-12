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
    ( withFile
    , fromFile
    , freeRWops
    ) where

import Control.Exception (bracket)
import Foreign (Ptr, FunPtr, finalizeForeignPtr, maybePeek, newForeignPtr, nullPtr)
import Foreign.C (withCString, CString)

import Graphics.UI.SDL.General (unwrapMaybe)
import Graphics.UI.SDL.Types (RWops, RWopsStruct)

withFile :: FilePath -> String -> (RWops -> IO a) -> IO a
withFile path mode = bracket (fromFile path mode) freeRWops

foreign import ccall unsafe "SDL_RWFromFile"
  sdlRWFromFile :: CString -> CString -> IO (Ptr RWopsStruct)

fromFile :: FilePath -> String -> IO RWops
fromFile filepath mode =
  withCString filepath $ \cPath ->
  withCString mode $ \cMode -> do
    rwops <- sdlRWFromFile cPath cMode
    if rwops == nullPtr
      then error "RWops.fromFile"
      else newForeignPtr sdlFreeRW_finalizer rwops

foreign import ccall unsafe "&SDL_FreeRW"
  sdlFreeRW_finalizer :: FunPtr (Ptr RWopsStruct -> IO ())

freeRWops :: RWops -> IO ()
freeRWops = finalizeForeignPtr
