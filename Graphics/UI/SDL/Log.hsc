#include "SDL.h"
module Graphics.UI.SDL.Log
    ( -- * SDL Routines
      log
    , logCritical
    , logDebug
    , logError
    , logGetOutputFunction
    , logGetPriority
    , logInfo
    , logMessage
    , logResetPriorities
    , logSetAllPriority
    , logSetOutputFunction
    , logSetPriority
    , logVerbose
    , logWarn
    , LogCategory (..)
    ) where

import Prelude hiding (log)

import Control.Applicative
import Data.Int
import Foreign
import Foreign.C
import Graphics.UI.SDL.StringUtilities (escapePrintf)

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_Log"
  sdlLog :: CString -> IO ()

log :: String -> IO ()
log message = withCString (escapePrintf message) sdlLog

--------------------------------------------------------------------------------
data LogCategory
  = LogCategoryApplication
  | LogCategoryError
  | LogCategorySystem
  | LogCategoryAudio
  | LogCategoryVideo
  | LogCategoryRender
  | LogCategoryInput
  | LogCategoryCustom
  deriving (Bounded, Enum, Eq, Read, Show)

logCategoryToInt :: LogCategory -> #{type int}
logCategoryToInt c = case c of
  LogCategoryApplication -> #{const SDL_LOG_CATEGORY_APPLICATION}
  LogCategoryError -> #{const SDL_LOG_CATEGORY_ERROR}
  LogCategorySystem -> #{const SDL_LOG_CATEGORY_SYSTEM}
  LogCategoryAudio -> #{const SDL_LOG_CATEGORY_AUDIO}
  LogCategoryVideo -> #{const SDL_LOG_CATEGORY_VIDEO}
  LogCategoryRender -> #{const SDL_LOG_CATEGORY_RENDER}
  LogCategoryInput -> #{const SDL_LOG_CATEGORY_INPUT}
  LogCategoryCustom -> #{const SDL_LOG_CATEGORY_CUSTOM}

logCategoryFromInt :: #{type int} -> LogCategory
logCategoryFromInt c = case c of
  #{const SDL_LOG_CATEGORY_APPLICATION} -> LogCategoryApplication
  #{const SDL_LOG_CATEGORY_ERROR} -> LogCategoryError
  #{const SDL_LOG_CATEGORY_SYSTEM} -> LogCategorySystem
  #{const SDL_LOG_CATEGORY_AUDIO} -> LogCategoryAudio
  #{const SDL_LOG_CATEGORY_VIDEO} -> LogCategoryVideo
  #{const SDL_LOG_CATEGORY_RENDER} -> LogCategoryRender
  #{const SDL_LOG_CATEGORY_INPUT} -> LogCategoryInput
  #{const SDL_LOG_CATEGORY_CUSTOM} -> LogCategoryCustom
  i -> error $ "Unexpected SDL_LogCategory: " ++ show i

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_LogCritical"
  sdlLogCritical :: #{type int} -> CString -> IO ()

logCritical :: LogCategory -> String -> IO ()
logCritical category message = withCString (escapePrintf message) $
  sdlLogCritical (logCategoryToInt category)

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_LogDebug"
  sdlLogDebug :: #{type int} -> CString -> IO ()

logDebug :: LogCategory -> String -> IO ()
logDebug category message = withCString (escapePrintf message) $
  sdlLogDebug (logCategoryToInt category)

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_LogError"
  sdlLogError :: #{type int} -> CString -> IO ()

logError :: LogCategory -> String -> IO ()
logError category message = withCString (escapePrintf message) $
  sdlLogError (logCategoryToInt category)

--------------------------------------------------------------------------------
data LogPriority
  = LogPriorityVerbose
  | LogPriorityDebug
  | LogPriorityInfo
  | LogPriorityWarn
  | LogPriorityError
  | LogPriorityCritical
  deriving (Bounded, Enum, Eq, Read, Show)
  
logPriorityToInt :: LogPriority -> #{type SDL_LogPriority}
logPriorityToInt p = case p of
  LogPriorityVerbose -> #{const SDL_LOG_PRIORITY_VERBOSE}
  LogPriorityDebug -> #{const SDL_LOG_PRIORITY_DEBUG}
  LogPriorityInfo -> #{const SDL_LOG_PRIORITY_INFO}
  LogPriorityWarn -> #{const SDL_LOG_PRIORITY_WARN}
  LogPriorityError -> #{const SDL_LOG_PRIORITY_ERROR}
  LogPriorityCritical -> #{const SDL_LOG_PRIORITY_CRITICAL}

logPriorityFromInt :: #{type SDL_LogPriority} -> LogPriority
logPriorityFromInt p = case p of
  #{const SDL_LOG_PRIORITY_VERBOSE} -> LogPriorityVerbose
  #{const SDL_LOG_PRIORITY_DEBUG} -> LogPriorityDebug
  #{const SDL_LOG_PRIORITY_INFO} -> LogPriorityInfo
  #{const SDL_LOG_PRIORITY_WARN} -> LogPriorityWarn
  #{const SDL_LOG_PRIORITY_ERROR} -> LogPriorityError
  #{const SDL_LOG_PRIORITY_CRITICAL} -> LogPriorityCritical
  i -> error $ "Unexpected SDL_LogPriority: " ++ show i

--------------------------------------------------------------------------------
type SDLOutputFunction = Ptr () -> #{type int} -> #{type SDL_LogPriority} -> CString -> IO ()

foreign import ccall unsafe "SDL_LogGetOutputFunction"
  sdlLogGetOutputFunction :: Ptr (FunPtr SDLOutputFunction) -> Ptr (Ptr ()) -> IO ()

foreign import ccall "dynamic"
  mkLogOutputFunction :: FunPtr SDLOutputFunction -> SDLOutputFunction

type OutputFunction = LogCategory -> LogPriority -> String -> IO ()

logGetOutputFunction :: IO OutputFunction
logGetOutputFunction =
  alloca $ \ofPtrLoc ->
  alloca $ \userDataPtr -> do
    sdlLogGetOutputFunction ofPtrLoc userDataPtr
    f <- mkLogOutputFunction <$> peek ofPtrLoc
    userData <- peek userDataPtr
    return $ \category priority message ->
      withCString message $ f userData (logCategoryToInt category) (logPriorityToInt priority)

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_LogGetPriority"
  sdlLogGetPriority :: #{type int} -> IO #{type SDL_LogPriority}

logGetPriority :: LogCategory -> IO LogPriority
logGetPriority = fmap logPriorityFromInt . sdlLogGetPriority . logCategoryToInt

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_LogInfo"
  sdlLogInfo :: #{type int} -> CString -> IO ()

logInfo :: LogCategory -> String -> IO ()
logInfo category message = withCString (escapePrintf message) $
  sdlLogInfo (logCategoryToInt category)

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_LogMessage"
  sdlLogMessage :: #{type int} -> #{type SDL_LogPriority} -> CString -> IO ()

logMessage :: LogCategory -> LogPriority -> String -> IO ()
logMessage category priority message = withCString (escapePrintf message) $
  sdlLogMessage (logCategoryToInt category) (logPriorityToInt priority)

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_LogResetPriorities"
  logResetPriorities :: IO ()
  
--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_LogSetAllPriority"
  sdlLogSetAllPriority :: #{type SDL_LogPriority} -> IO ()

logSetAllPriority :: LogPriority -> IO ()
logSetAllPriority = sdlLogSetAllPriority . logPriorityToInt

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_LogSetOutputFunction"
  sdlLogSetOutputFunction :: FunPtr (SDLOutputFunction) -> Ptr () -> IO ()
  
foreign import ccall "wrapper"
   mkSDLLogOutputFunction :: SDLOutputFunction -> IO (FunPtr SDLOutputFunction)
   
logSetOutputFunction :: OutputFunction -> IO ()
logSetOutputFunction f = do
  f' <- mkSDLLogOutputFunction $ \_ c p m ->
          peekCString m >>= f (logCategoryFromInt c) (logPriorityFromInt p)
  sdlLogSetOutputFunction f' nullPtr

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_LogSetPriority"
  sdlLogSetPriority :: #{type int} -> #{type SDL_LogPriority} -> IO ()

logSetPriority :: LogCategory -> LogPriority -> IO ()
logSetPriority c p = sdlLogSetPriority (logCategoryToInt c) (logPriorityToInt p)

--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_LogVerbose"
  sdlLogVerbose :: #{type int} -> CString -> IO ()

logVerbose :: LogCategory -> String -> IO ()
logVerbose category message = withCString (escapePrintf message) $
  sdlLogVerbose (logCategoryToInt category)
  
--------------------------------------------------------------------------------
foreign import ccall unsafe "SDL_LogWarn"
  sdlLogWarn :: #{type int} -> CString -> IO ()

logWarn :: LogCategory -> String -> IO ()
logWarn category message = withCString (escapePrintf message) $
  sdlLogWarn (logCategoryToInt category)
