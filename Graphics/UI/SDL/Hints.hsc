#include "SDL.h"
module Graphics.UI.SDL.Hints
    ( -- * SDL Routines
      addHintCallback
    , clearHints
    , delHintCallback
    , getHint
    , setHint
    , setHintWithPriority
    , HintCallback
    ) where

import Control.Applicative
import Control.Monad ((>=>), join, void)
import Data.Word
import Foreign hiding (void)
import Foreign.C
import Graphics.UI.SDL.Utilities (sdlBoolToBool)

--------------------------------------------------------------------------------
type HintCallbackF = Ptr () -> CString -> CString -> CString -> IO ()

foreign import ccall "wrapper"
  mkHintCallback :: HintCallbackF -> IO (FunPtr HintCallbackF)

newtype HintCallback = HintCallback (FunPtr HintCallbackF)

foreign import ccall "SDL_AddHintCallback"
  sdlAddHintCallback :: CString -> FunPtr HintCallbackF -> Ptr () -> IO ()

addHintCallback :: String -> (String -> String -> String -> IO a) -> IO HintCallback
addHintCallback hintName callback = withCString hintName $ \cHintName -> do
  cb <- mkHintCallback $ \_ hint oldV newV -> void $ join $
    callback <$> peekCString hint <*> peekCString oldV <*> peekCString newV
  HintCallback cb <$ sdlAddHintCallback cHintName cb nullPtr

--------------------------------------------------------------------------------
foreign import ccall "SDL_DelHintCallback"
  sdlDelHintCallback :: CString -> FunPtr HintCallbackF -> Ptr () -> IO ()

delHintCallback :: String -> HintCallback -> IO ()
delHintCallback hintName (HintCallback f) = withCString hintName $ \cHintName -> do
  sdlDelHintCallback cHintName f nullPtr

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_ClearHints"
  clearHints :: IO ()

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_GetHint"
  sdlGetHint :: CString -> IO CString

getHint :: String -> IO (Maybe String)
getHint hint = withCString hint (sdlGetHint >=> maybePeek peekCString)

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_SetHint"
  sdlSetHint :: CString -> CString -> IO Bool

setHint :: String -> String -> IO Bool
setHint k v =
  withCString k $ \cK ->
  withCString v $ \cV ->
  sdlSetHint cK cV

--------------------------------------------------------------------------------
data HintPriority = HintDefault | HintNormal | HintOverride

foreign import ccall unsafe "SDL_SetHintWithPriority"
  sdlSetHintWithPriority :: CString -> CString -> #{type SDL_HintPriority} -> IO #{type SDL_bool}

setHintWithPriority :: String -> String -> HintPriority -> IO Bool
setHintWithPriority k v priority =
  withCString k $ \cK ->
  withCString v $ \cV ->
  fmap sdlBoolToBool $ sdlSetHintWithPriority cK cV $
    case priority of
      HintDefault -> #{const SDL_HINT_DEFAULT}
      HintNormal -> #{const SDL_HINT_NORMAL}
      HintOverride -> #{const SDL_HINT_OVERRIDE}
