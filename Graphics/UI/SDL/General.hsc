#include "SDL.h"
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.SDL.General
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Graphics.UI.SDL.General
    ( -- * Error Handling
      clearError
    , getError
    , setError

      -- * Simple Message Box
    , showSimpleMessageBox
    , MessageBoxType(..)

      -- * Hints
    , clearHints
    , getHint
    , setHint
    , setHintWithPriority
    , HintPriority(..)

    -- ** Hint Callbacks
    , HintCallback
    , addHintCallback
    , delHintCallback

      -- * Timers
    , addTimer
    , removeTimer

       -- * Utilities
    , failWithError
    , unwrapBool
    , unwrapMaybe
    , unwrapInt
    , handleError
    , handleErrorI
    ) where

import Prelude hiding (init)
import Control.Applicative
import Control.Exception (bracket_)
import Control.Monad ((>=>), join, void, when)
import Data.Int (Int32)
import Data.Maybe (fromMaybe)
import Data.Word (Word32)
import Foreign.C (CString, CUInt(..), peekCString, withCString)
import Foreign.ForeignPtr.Safe (withForeignPtr)
import Foreign.Ptr (FunPtr, Ptr, nullPtr)

import Graphics.UI.SDL.Types (WindowStruct, Window)
import Graphics.UI.SDL.Utilities (toBitmask, fromBitmask)



--instance Bounded InitFlag where
--      minBound = InitTimer
--      maxBound = InitNoParachute

-- XXX: Use CUInt?


unwrapMaybe :: String -> IO (Maybe a) -> IO a
unwrapMaybe errMsg action
    = do val <- action
         case val of
           Just a -> return a
           Nothing -> failWithError errMsg

unwrapInt :: (Int -> Bool) -> String -> IO Int -> IO Int
unwrapInt fn errMsg action
    = do val <- action
         if fn val
            then return val
            else failWithError errMsg

unwrapBool :: String -> IO Bool -> IO ()
unwrapBool errMsg action
    = do val <- action
         case val of
           True -> return ()
           False -> failWithError errMsg


foreign import ccall unsafe "SDL_GetError" sdlGetError :: IO CString

-- | Returns a string containing the last error. Nothing if no error.
getError :: IO (Maybe String)
getError = sdlGetError >>= maybeString

foreign import ccall unsafe "SDL_ClearError" clearError :: IO ()

foreign import ccall unsafe "SDL_SetError" sdlSetError :: CString -> IO Int

-- TODO This should escape %
setError :: String -> IO ()
setError errstr = withCString errstr $ void . sdlSetError

failWithError :: String -> IO a
failWithError msg
    = do err <- fmap (fromMaybe "No SDL error") getError
         ioError $ userError $ msg ++ "\nSDL message: " ++ err

foreign import ccall unsafe "SDL_ShowSimpleMessageBox" sdlShowSimpleMessageBox :: CUInt -> CString -> CString -> Ptr WindowStruct -> IO ()

data MessageBoxType = Error | Warning | Information

-- | Show a message box.
showSimpleMessageBox :: MessageBoxType -> String -> String -> Maybe Window -> IO ()
showSimpleMessageBox flag title msg parent =
  withCString title $ \ctitle ->
  withCString msg $ \cmsg ->
  let go parent' = sdlShowSimpleMessageBox cflag ctitle cmsg parent' in fromMaybe (go nullPtr) (fmap (`withForeignPtr` go) parent)
  where
    cflag = case flag of
      Error -> #{const SDL_MESSAGEBOX_ERROR}
      Warning -> #{const SDL_MESSAGEBOX_WARNING}
      Information -> #{const SDL_MESSAGEBOX_INFORMATION}

foreign import ccall unsafe "SDL_ClearHints" sdlClearHints :: IO ()

-- | Clear all hints.
clearHints :: IO ()
clearHints = sdlClearHints

foreign import ccall unsafe "SDL_GetHint" sdlGetHint :: CString -> IO CString
-- | Returns 'Just' the string value of a hint or 'Nothing' if the hint is not
-- set.
getHint :: String -> IO (Maybe String)
getHint hint = withCString hint (sdlGetHint >=> maybeString)

foreign import ccall unsafe "SDL_SetHint" sdlSetHint :: CString -> CString -> IO Bool
-- | Set a hint with normal priority. Returns 'True' if the hint was set,
-- 'False' otherwise.
setHint :: String -> String -> IO Bool
setHint k v =
  withCString k $ \cK ->
  withCString v $ \cV ->
  sdlSetHint cK cV

data HintPriority = HintDefault | HintNormal | HintOverride

foreign import ccall unsafe "SDL_SetHintWithPriority" sdlSetHintWithPriority
  :: CString -> CString -> Int -> IO Bool

-- | Set a hint with normal priority. Returns 'True' if the hint was set,
-- 'False' otherwise.
setHintWithPriority :: String -> String -> HintPriority -> IO Bool
setHintWithPriority k v priority =
  withCString k $ \cK ->
  withCString v $ \cV ->
  sdlSetHintWithPriority cK cV $
    case priority of
      HintDefault -> #{const SDL_HINT_DEFAULT}
      HintNormal -> #{const SDL_HINT_NORMAL}
      HintOverride -> #{const SDL_HINT_OVERRIDE}

foreign import ccall "wrapper"
  mkHintCallback :: (Ptr () -> CString -> CString -> CString -> IO ())
                 -> IO (FunPtr (Ptr () -> CString -> CString -> CString -> IO ()))

newtype HintCallback = HintCallback (FunPtr (Ptr () -> CString -> CString -> CString -> IO ()))

foreign import ccall "SDL_AddHintCallback"
  sdlAddHintCallback :: CString -> FunPtr (Ptr () -> CString -> CString -> CString -> IO ()) -> Ptr () -> IO ()

addHintCallback :: String -> (String -> String -> String -> IO a) -> IO HintCallback
addHintCallback hintName callback = withCString hintName $ \cHintName -> do
  cb <- mkHintCallback $ \_ hint old new -> void $ join $
    callback <$> peekCString hint <*> peekCString old <*> peekCString new
  HintCallback cb <$ sdlAddHintCallback cHintName cb nullPtr

foreign import ccall "SDL_DelHintCallback"
  sdlDelHintCallback :: CString -> FunPtr (Ptr () -> CString -> CString -> CString -> IO ()) -> Ptr () -> IO ()

delHintCallback :: String -> HintCallback -> IO ()
delHintCallback hintName (HintCallback f) = withCString hintName $ \cHintName -> do
  sdlDelHintCallback cHintName f nullPtr

foreign import ccall "wrapper"
  mkTimerCallback :: (Word32 -> Ptr () -> IO Word32)
     -> IO (FunPtr (Word32 -> Ptr () -> IO Word32))

newtype TimerCallback = TimerCallback #{type SDL_TimerID}

foreign import ccall "SDL_AddTimer"
  sdlAddTimer :: Word32 -> FunPtr (Word32 -> Ptr () -> IO Word32) -> Ptr () -> IO #{type SDL_TimerID}

data TimerTermination = CancelTimer | ContinueTimer

addTimer :: Word32 -> (Word32 -> IO TimerTermination) -> IO TimerCallback
addTimer interval callback = do
  cb <- mkTimerCallback $ \passed _ -> do
    termination <- callback passed
    case termination of
      CancelTimer -> return 0
      ContinueTimer -> return 1

  TimerCallback <$> sdlAddTimer interval cb nullPtr

foreign import ccall "SDL_RemoveTimer"
  sdlRemoveTimer :: #{type SDL_TimerID} -> IO #{type SDL_bool}

removeTimer :: TimerCallback -> IO Bool
removeTimer (TimerCallback tId) = do
  (== #{const SDL_TRUE}) <$> sdlRemoveTimer tId

maybeString :: CString -> IO (Maybe String)
maybeString = fmap maybeString . peekCString
  where maybeString str | null str = Nothing
                        | otherwise = Just str


handleError fname ptr fn
  | ptr == nullPtr = (\err -> error $ fname ++ ": " ++ show err) =<< getError
  | otherwise      = fn ptr
{-# INLINE handleError #-}

handleErrorI fname i fn
  | i < 0     = fn i
  | otherwise =  (\err -> error $ fname ++ ": " ++ show err) =<< getError
{-# INLINE handleErrorI #-}


