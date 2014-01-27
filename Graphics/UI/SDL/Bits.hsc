#include "SDL.h"
module Graphics.UI.SDL.Bits
  ( mostSignificantBitIndex32
  ) where

import Foreign

foreign import ccall safe "SDL_MostSignificantBitIndex32_Wrapper"
  mostSignificantBitIndex32 :: #{type Uint32} -> #{type int}

