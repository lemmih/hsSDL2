-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.SDL.TTF.General
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Graphics.UI.SDL.TTF.General
    ( init
    , wasInit
    , quit
    ) where

import Foreign

-- import Graphics.UI.SDL.General (failWithError)

import Prelude hiding (init)

-- int TTF_Init()
foreign import ccall unsafe "TTF_Init" ttfInit :: IO Int
init :: IO Bool
init = fmap (not.toBool) ttfInit


--int TTF_WasInit()
foreign import ccall unsafe "TTF_WasInit" ttfWasInit :: IO Int
wasInit :: IO Bool
wasInit = fmap toBool ttfWasInit

-- void TTF_Quit()
foreign import ccall unsafe "TTF_Quit" quit :: IO ()

