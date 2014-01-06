#include "SDL.h"
module Graphics.UI.SDL.Clipboard
    ( getClipboardText
    , hasClipboardText
    , setClipboardText
    ) where

import Control.Applicative
import Data.ByteString (useAsCString, packCString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Foreign
import Foreign.C
import Graphics.UI.SDL.Utilities (fatalSDLNull, fatalSDLBool, sdlBoolToBool, sdlFree)

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_GetClipboardText"
  sdlGetClipboardText :: IO CString

-- | Use this function to get UTF-8 text from the clipboard.
getClipboardText :: IO Text
getClipboardText = do
  cstr <- fatalSDLNull "SDL_GetClipboardText" sdlGetClipboardText
  bs <- packCString cstr
  sdlFree cstr
  return $! decodeUtf8 bs

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_SetClipboardText"
  sdlSetClipboardText :: CString -> IO #{type int}

-- | Use this function to put UTF-8 text into the clipboard.
setClipboardText :: Text -> IO ()
setClipboardText txt =
  useAsCString (encodeUtf8 txt) $ fatalSDLBool "SDL_SetClipboardText" .
    sdlSetClipboardText

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_HasClipboardText"
  sdlHasClipboardText :: IO #{type SDL_bool}

-- | Use this function to return a flag indicating whether the clipboard
--   exists and contains a text string that is non-empty.
hasClipboardText :: IO Bool
hasClipboardText = sdlBoolToBool <$> sdlHasClipboardText
