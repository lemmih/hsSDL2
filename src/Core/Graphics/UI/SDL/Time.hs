-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.SDL.Time
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Graphics.UI.SDL.Time where


import Foreign

-- Uint32 SDL_GetTicks(void);
foreign import ccall unsafe "SDL_GetTicks" getTicks :: IO Word32

-- void SDL_Delay(Uint32 ms);
foreign import ccall unsafe "SDL_Delay" delay :: Word32 -> IO ()

