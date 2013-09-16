#include "SDL.h"
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
    , closeRWops
    ) where

import Control.Exception (bracket)
import Foreign
import Foreign.C

import Graphics.UI.SDL.General (getError)
import Graphics.UI.SDL.Types (RWops, RWopsStruct)

withFile :: FilePath -> String -> (RWops -> IO a) -> IO a
withFile path mode = bracket (fromFile path mode) closeRWops

foreign import ccall unsafe "SDL_RWFromFile"
  sdlRWFromFile :: CString -> CString -> IO (Ptr RWopsStruct)

fromFile :: FilePath -> String -> IO RWops
fromFile filepath mode =
  withCString filepath $ \cPath ->
  withCString mode $ \cMode -> do
    rwops <- sdlRWFromFile cPath cMode
    if rwops == nullPtr
      then getError >>= error . ("RWops.fromFile: " ++) . show
      else do
        finalizer <- #{peek SDL_RWops, close} rwops
        newForeignPtr finalizer rwops

closeRWops :: RWops -> IO ()
closeRWops = finalizeForeignPtr
