-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.SDL.Events
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Graphics.UI.SDL.Events
    ( Event (..)
    , SDLEvent (..)
    , MouseButton (..)
    , enableKeyRepeat
    , enableUnicode
    , queryUnicodeState
    , getKeyName
    , getMouseState
    , getRelativeMouseState
    , getModState
    , setModState
    , tryPushEvent
    , pushEvent
    , pollEvent
    , waitEvent
    , pumpEvents
    , enableEvent
    , queryEventState
    , getAppState
    ) where

#include <SDL.h>

import Foreign
import Foreign.C
import Data.Bits

import Graphics.UI.SDL.Keysym
import Graphics.UI.SDL.Utilities
import Graphics.UI.SDL.General
import Graphics.UI.SDL.Video

data SDLEvent = SDLNoEvent
              | SDLActiveEvent
              | SDLKeyDown
              | SDLKeyUp
              | SDLMouseMotion
              | SDLMouseButtonDown
              | SDLMouseButtonUp
              | SDLJoyAxisMotion
              | SDLJoyBallMotion
              | SDLJoyHatMotion
              | SDLJoyButtonDown
              | SDLJoyButtonUp
              | SDLQuit
              | SDLSysWMEvent
              | SDLVideoResize
              | SDLVideoExpose
              | SDLUserEvent
              | SDLNumEvents
    deriving (Eq, Ord, Show)
instance Bounded SDLEvent where
    minBound = SDLNoEvent
    maxBound = SDLNumEvents

fromSDLEvent :: SDLEvent -> Word8
fromSDLEvent SDLNoEvent = #{const SDL_NOEVENT}
fromSDLEvent SDLActiveEvent = #{const SDL_ACTIVEEVENT}
fromSDLEvent SDLKeyDown = #{const SDL_KEYDOWN}
fromSDLEvent SDLKeyUp = #{const SDL_KEYUP}
fromSDLEvent SDLMouseMotion = #{const SDL_MOUSEMOTION}
fromSDLEvent SDLMouseButtonDown = #{const SDL_MOUSEBUTTONDOWN}
fromSDLEvent SDLMouseButtonUp = #{const SDL_MOUSEBUTTONUP}
fromSDLEvent SDLJoyAxisMotion = #{const SDL_JOYAXISMOTION}
fromSDLEvent SDLJoyBallMotion = #{const SDL_JOYBALLMOTION}
fromSDLEvent SDLJoyHatMotion = #{const SDL_JOYHATMOTION}
fromSDLEvent SDLJoyButtonDown = #{const SDL_JOYBUTTONDOWN}
fromSDLEvent SDLJoyButtonUp = #{const SDL_JOYBUTTONUP}
fromSDLEvent SDLQuit = #{const SDL_QUIT}
fromSDLEvent SDLSysWMEvent = #{const SDL_SYSWMEVENT}
fromSDLEvent SDLVideoResize = #{const SDL_VIDEORESIZE}
fromSDLEvent SDLVideoExpose = #{const SDL_VIDEOEXPOSE}
fromSDLEvent SDLUserEvent = #{const SDL_USEREVENT}
fromSDLEvent SDLNumEvents = #{const SDL_NUMEVENTS}

toSDLEvent :: Word8 -> SDLEvent
toSDLEvent #{const SDL_NOEVENT} = SDLNoEvent
toSDLEvent #{const SDL_ACTIVEEVENT} = SDLActiveEvent
toSDLEvent #{const SDL_KEYDOWN} = SDLKeyDown
toSDLEvent #{const SDL_KEYUP} = SDLKeyUp
toSDLEvent #{const SDL_MOUSEMOTION} = SDLMouseMotion
toSDLEvent #{const SDL_MOUSEBUTTONDOWN} = SDLMouseButtonDown
toSDLEvent #{const SDL_MOUSEBUTTONUP} = SDLMouseButtonUp
toSDLEvent #{const SDL_JOYAXISMOTION} = SDLJoyAxisMotion
toSDLEvent #{const SDL_JOYBALLMOTION} = SDLJoyBallMotion
toSDLEvent #{const SDL_JOYHATMOTION} = SDLJoyHatMotion
toSDLEvent #{const SDL_JOYBUTTONDOWN} = SDLJoyButtonDown
toSDLEvent #{const SDL_JOYBUTTONUP} = SDLJoyButtonUp
toSDLEvent #{const SDL_QUIT} = SDLQuit
toSDLEvent #{const SDL_SYSWMEVENT} = SDLSysWMEvent
toSDLEvent #{const SDL_VIDEORESIZE} = SDLVideoResize
toSDLEvent #{const SDL_VIDEOEXPOSE} = SDLVideoExpose
toSDLEvent #{const SDL_USEREVENT} = SDLUserEvent
toSDLEvent #{const SDL_NUMEVENTS} = SDLNumEvents


data Event
    = NoEvent
    | GotFocus [Focus]
    | LostFocus [Focus]
    | GotApplicationFocus
    | LostApplicationFocus
    | KeyDown !Keysym
    | KeyUp !Keysym
    | MouseMotion !Word16 !Word16
    | MouseButtonDown !Word16
                      !Word16
                      !MouseButton
    | MouseButtonUp !Word16
                    !Word16
                    !MouseButton
    | VideoResize !Int !Int
    | VideoExpose
    | Quit
    | User !UserEventID
           !Int
           !(Ptr ())
           !(Ptr ())
    | Unknwon
      deriving (Show,Eq)

data MouseButton
    = ButtonLeft
    | ButtonMiddle
    | ButtonRight
    | ButtonWheelUp
    | ButtonWheelDown
      deriving (Show,Eq)

instance Enum MouseButton where
    toEnum #{const SDL_BUTTON_LEFT} = ButtonLeft
    toEnum #{const SDL_BUTTON_MIDDLE} = ButtonMiddle
    toEnum #{const SDL_BUTTON_RIGHT} = ButtonRight
    toEnum #{const SDL_BUTTON_WHEELUP} = ButtonWheelUp
    toEnum #{const SDL_BUTTON_WHEELDOWN} = ButtonWheelDown
    fromEnum ButtonLeft = #{const SDL_BUTTON_LEFT}
    fromEnum ButtonMiddle = #{const SDL_BUTTON_MIDDLE}
    fromEnum ButtonRight = #{const SDL_BUTTON_RIGHT}
    fromEnum ButtonWheelUp = #{const SDL_BUTTON_WHEELUP}
    fromEnum ButtonWheelDown = #{const SDL_BUTTON_WHEELDOWN}

data Focus
    = MouseFocus
    | InputFocus
    | ApplicationFocus
      deriving (Show,Eq,Ord)

instance Bounded Focus where
    minBound = MouseFocus
    maxBound = ApplicationFocus

instance Enum Focus where
    fromEnum MouseFocus = #{const SDL_APPMOUSEFOCUS}
    fromEnum InputFocus = #{const SDL_APPINPUTFOCUS}
    fromEnum ApplicationFocus = #{const SDL_APPACTIVE}
    toEnum #{const SDL_APPMOUSEFOCUS} = MouseFocus
    toEnum #{const SDL_APPINPUTFOCUS} = InputFocus
    toEnum #{const SDL_APPACTIVE} = ApplicationFocus
    succ MouseFocus = InputFocus
    succ InputFocus = ApplicationFocus
    pred InputFocus = MouseFocus
    pred ApplicationFocus = InputFocus
    enumFromTo x y | x > y = []
                   | x == y = [y]
                   | True = x : enumFromTo (succ x) y

data UserEventID
    = UID0 | UID1 | UID2 | UID3 | UID4 | UID5 | UID6 | UID7
      deriving (Show,Eq,Enum)
toEventType :: UserEventID -> Word8
toEventType eid = fromIntegral (fromEnum eid + #{const SDL_USEREVENT})

handleActiveEvent :: Ptr Event -> IO Event
handleActiveEvent ptr
    = do gain <- fmap toBool ((#{peek SDL_ActiveEvent, gain} ptr) :: IO Word8)
         state <- #{peek SDL_ActiveEvent, state} ptr :: IO Word8
         return $! (if gain then GotFocus else LostFocus) (fromBitmask (fromIntegral state))

handleKey :: (Keysym -> Event) -> Ptr Event -> IO Event
handleKey mkEvent ptr
    = do keysym <- #{peek SDL_KeyboardEvent, keysym} ptr
         return $! mkEvent keysym

handleMouseMotion :: Ptr Event -> IO Event
handleMouseMotion ptr
    = do x <- #{peek SDL_MouseMotionEvent, x} ptr
         y <- #{peek SDL_MouseMotionEvent, y} ptr
         return $! MouseMotion x y

handleMouse :: (Word16 -> Word16 -> MouseButton -> Event) -> Ptr Event -> IO Event
handleMouse mkEvent ptr
    = do b <- #{peek SDL_MouseButtonEvent, button} ptr
         x <- #{peek SDL_MouseButtonEvent, x} ptr
         y <- #{peek SDL_MouseButtonEvent, y} ptr
         return $! mkEvent x y (toEnum (fromIntegral (b::Word8)))

handleResize :: Ptr Event -> IO Event
handleResize ptr
    = do w <- #{peek SDL_ResizeEvent, w} ptr
         h <- #{peek SDL_ResizeEvent, h} ptr
         return $! VideoResize w h

getEventType :: Event -> Word8
getEventType = fromIntegral . fromSDLEvent . eventToSDLEvent

eventToSDLEvent :: Event -> SDLEvent
eventToSDLEvent NoEvent = SDLNoEvent
eventToSDLEvent (GotFocus _) = SDLActiveEvent
eventToSDLEvent (LostFocus _) = SDLActiveEvent
eventToSDLEvent (KeyDown _) = SDLKeyDown
eventToSDLEvent (KeyUp _) = SDLKeyUp
eventToSDLEvent (MouseMotion _ _) = SDLMouseMotion
eventToSDLEvent (MouseButtonDown _ _ _) = SDLMouseButtonDown
eventToSDLEvent (MouseButtonUp _ _ _) = SDLMouseButtonUp
eventToSDLEvent Quit = SDLQuit
eventToSDLEvent (VideoResize _ _) = SDLVideoResize
eventToSDLEvent VideoExpose = SDLVideoExpose

pokeActiveEvent :: Ptr Event -> Word8 -> [Focus] -> IO ()
pokeActiveEvent ptr gain focus
    = do #{poke SDL_ActiveEvent, gain} ptr gain
         #{poke SDL_ActiveEvent, state} ptr (fromIntegral (toBitmask focus) :: Word8)

pokeKey :: Ptr Event -> Word8 -> Keysym -> IO ()
pokeKey ptr state keysym
    = do #{poke SDL_KeyboardEvent, state} ptr state
         #{poke SDL_KeyboardEvent, keysym} ptr keysym

pokeMouseMotion :: Ptr Event -> Word16 -> Word16 -> IO ()
pokeMouseMotion ptr x y
    = do #{poke SDL_MouseMotionEvent, x} ptr x
         #{poke SDL_MouseMotionEvent, y} ptr y

pokeMouseButton :: Ptr Event -> Word8 -> Word16 -> Word16 -> MouseButton -> IO ()
pokeMouseButton ptr state x y b
    = do #{poke SDL_MouseButtonEvent, x} ptr x
         #{poke SDL_MouseButtonEvent, y} ptr y
         #{poke SDL_MouseButtonEvent, state} ptr state
         #{poke SDL_MouseButtonEvent, button} ptr (fromIntegral (fromEnum b) :: Word8)

pokeResize :: Ptr Event -> Int -> Int -> IO ()
pokeResize ptr w h
    = do #{poke SDL_ResizeEvent, w} ptr w
         #{poke SDL_ResizeEvent, h} ptr h

instance Storable Event where
    sizeOf = const (#{size SDL_Event})
    alignment = const 4
    poke ptr event
        = do pokeByteOff ptr 0 (getEventType event)
             case event of
               NoEvent               -> return ()
               GotFocus focus        -> pokeActiveEvent ptr 1 focus
               LostFocus focus       -> pokeActiveEvent ptr 0 focus
               KeyDown keysym        -> pokeKey ptr #{const SDL_PRESSED} keysym
               KeyUp keysym          -> pokeKey ptr #{const SDL_RELEASED} keysym
               MouseMotion x y       -> pokeMouseMotion ptr x y
               MouseButtonDown x y b -> pokeMouseButton ptr #{const SDL_PRESSED} x y b
               MouseButtonUp x y b   -> pokeMouseButton ptr #{const SDL_RELEASED} x y b
               Quit                  -> return ()
               VideoResize w h       -> pokeResize ptr w h
               VideoExpose           -> return ()
               e                     -> failWithError $ "Unhandled eventtype: " ++ show e
    peek ptr
        = do eventType <- peekByteOff ptr 0
             case toSDLEvent eventType of
               SDLNoEvent         -> return NoEvent
               SDLActiveEvent     -> handleActiveEvent ptr
               SDLKeyDown         -> handleKey KeyDown ptr
               SDLKeyUp           -> handleKey KeyUp ptr
               SDLMouseMotion     -> handleMouseMotion ptr
               SDLMouseButtonDown -> handleMouse MouseButtonDown ptr
               SDLMouseButtonUp   -> handleMouse MouseButtonUp ptr
               {-           SDLJoyAxisMotion
                            SDLJoyBallMotion
                            SDLJoyHatMotion
                            SDLJoyButtonDown
                            SDLJoyButtonUp -}
               SDLQuit            -> return Quit
--           SDLSysWMEvent
               SDLVideoResize     -> handleResize ptr
               SDLVideoExpose     -> return VideoExpose
--           SDLUserEvent
--           SDLNumEvents           
               e                  -> failWithError $ "Unhandled eventtype: " ++ show e

-- int SDL_EnableKeyRepeat(int delay, int interval);
foreign import ccall unsafe "SDL_EnableKeyRepeat" sdlEnableKeyRepeat :: Int -> Int -> IO Int

-- | Sets keyboard repeat rate. Returns @False@ on error.
enableKeyRepeat :: Int -- ^ Initial delay. @0@ to disable.
                -> Int -- ^ Interval.
                -> IO Bool
enableKeyRepeat delay interval
    = intToBool (-1) (sdlEnableKeyRepeat delay interval)

-- int SDL_EnableUNICODE(int enable);
foreign import ccall unsafe "SDL_EnableUNICODE" sdlEnableUnicode :: Int -> IO Int

-- | Enables or disables unicode translation.
enableUnicode :: Bool -> IO ()
enableUnicode enable = sdlEnableUnicode (fromToggle toggle) >>
                       return ()
    where toggle = case enable of
                     True -> Enable
                     False -> Disable

-- | Returns the current state of unicode translation. See also 'enableUnicode'.
queryUnicodeState :: IO Bool
queryUnicodeState = fmap toBool (sdlEnableUnicode (fromToggle Query))

-- char *SDL_GetKeyName(SDLKey key);
foreign import ccall unsafe "SDL_GetKeyName" sdlGetKeyName :: Int -> IO CString

-- | Gets the name of an SDL virtual keysym.
getKeyName :: SDLKey -> String
getKeyName key = unsafePerformIO $
                 sdlGetKeyName (fromEnum key) >>= peekCString

-- SDLMod SDL_GetModState(void);
foreign import ccall unsafe "SDL_GetModState" sdlGetModState :: IO Int

-- | Gets the state of modifier keys.
getModState :: IO [Modifier]
getModState = fmap fromBitmask sdlGetModState

-- void SDL_SetModState(SDLMod modstate);
foreign import ccall unsafe "SDL_SetModState" sdlSetModState :: Int -> IO ()

-- | Sets the internal state of modifier keys.
setModState :: [Modifier] -> IO ()
setModState = sdlSetModState . toBitmask

mousePressed :: Word8 -> MouseButton -> Bool
mousePressed mask b
    = mask .&. (1 `shiftL` num) /= 0
    where num = fromIntegral (fromEnum b)
                  

-- Uint8 SDL_GetMouseState(int *x, int *y);
foreign import ccall "SDL_GetMouseState" sdlGetMouseState :: Ptr Int -> Ptr Int -> IO Word8
foreign import ccall "SDL_GetRelativeMouseState" sdlGetRelativeMouseState :: Ptr Int -> Ptr Int -> IO Word8

-- | Retrieves the current state of the mouse. Returns (X position, Y position, pressed buttons).
getMouseState :: IO (Int, Int, [MouseButton])
getMouseState = mouseStateGetter sdlGetMouseState

-- | Retrieve the current state of the mouse. Like 'getMouseState' except that X and Y are
--   set to the change since last call to getRelativeMouseState.
getRelativeMouseState :: IO (Int, Int, [MouseButton])
getRelativeMouseState = mouseStateGetter sdlGetRelativeMouseState

mouseStateGetter :: (Ptr Int -> Ptr Int -> IO Word8) -> IO  (Int, Int, [MouseButton])
mouseStateGetter getter
    = alloca $ \xPtr ->
      alloca $ \yPtr ->
      do ret <- getter xPtr yPtr
         [x,y] <- mapM peek [xPtr,yPtr]
         return (x,y,filter (mousePressed ret) [ButtonLeft
                                               ,ButtonMiddle
                                               ,ButtonRight
                                               ,ButtonWheelUp
                                               ,ButtonWheelDown])



-- int SDL_PollEvent(SDL_Event *event);
foreign import ccall "SDL_PollEvent" sdlPollEvent :: Ptr Event -> IO Int

-- | Polls for currently pending events.
pollEvent :: IO Event
pollEvent 
    = alloca poll
    where poll ptr
              = do ret <- sdlPollEvent ptr
                   case ret of
                     0 -> return NoEvent
                     _ -> do event <- peek ptr
                             case event of
                               NoEvent -> poll ptr
                               _ -> return event

-- void SDL_PumpEvents(void);
foreign import ccall unsafe "SDL_PumpEvents" pumpEvents :: IO ()

-- int SDL_PushEvent(SDL_Event *event);
foreign import ccall unsafe "SDL_PushEvent" sdlPushEvent :: Ptr Event -> IO Int

-- | Pushes an event onto the event queue. Returns @False@ on error.
tryPushEvent :: Event -> IO Bool
tryPushEvent event
    = new event >>= (fmap (0==) . sdlPushEvent)

-- | Pushes an event onto the event queue. Throws an exception on error.
pushEvent :: Event -> IO ()
pushEvent = unwrapBool "SDL_PushEvent" . tryPushEvent

-- int SDL_WaitEvent(SDL_Event *event);
foreign import ccall unsafe "SDL_WaitEvent" sdlWaitEvent :: Ptr Event -> IO Int

-- | Waits indefinitely for the next available event.
waitEvent :: IO Event
waitEvent 
    = alloca wait
    where wait ptr
              = do ret <- sdlWaitEvent ptr
                   case ret of
                     0 -> failWithError "SDL_WaitEvent"
                     _ -> do event <- peek ptr
                             case event of
                               NoEvent -> wait ptr
                               _ -> return event

-- Uint8 SDL_EventState(Uint8 type, int state);
foreign import ccall unsafe "SDL_EventState" sdlEventState :: Word8 -> Int -> IO Word8
enableEvent :: SDLEvent -> Bool -> IO ()
enableEvent event on
    = sdlEventState (fromSDLEvent event) (fromToggle state) >> return ()
    where state
              | on = Enable
              | otherwise = Disable

queryEventState :: SDLEvent -> IO Bool
queryEventState event
    = fmap (==1) (sdlEventState (fromSDLEvent event) (fromToggle Query))

-- Uint8 SDL_GetAppState(void);
foreign import ccall unsafe "SDL_GetAppState" sdlGetAppState :: IO Word8

-- | Gets the state of the application.
getAppState :: IO [Focus]
getAppState = fmap (fromBitmask . fromIntegral) sdlGetAppState

