#include "SDL.h"
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
    , UserEventID (..)
    , MouseButton (..)
    , Focus(..)
    , toSafePtr
    , tryFromSafePtr
    , fromSafePtr
    , typeOfSafePtr
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
    , waitEventBlocking
    , pumpEvents
    , enableEvent
    , queryEventState
    , getAppState
    ) where

import Foreign (Int16, Word8, Word16, Word32, Ptr,
               Storable(poke, sizeOf, alignment, peekByteOff, pokeByteOff, peek),
               unsafePerformIO, toBool, new, alloca)
import Foreign.C (peekCString, CString)
import Data.Bits (Bits((.&.), shiftL))
import Control.Concurrent (threadDelay)
import Prelude hiding (Enum(..))
import qualified Prelude (Enum(..))

import Foreign.StablePtr (newStablePtr,castStablePtrToPtr,castPtrToStablePtr,deRefStablePtr)
import Data.Typeable (Typeable(typeOf),TypeRep)

import Graphics.UI.SDL.Keysym (SDLKey, Modifier, Keysym)
import Graphics.UI.SDL.Utilities (Enum(..), intToBool, toBitmask, fromBitmask)
import Graphics.UI.SDL.General (unwrapBool, failWithError)
import Graphics.UI.SDL.Video (Toggle(..), fromToggle)

-- |Low level event structure keeping a one-to-one relation with the C event structure.
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
              | SDLUserEvent Word8
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
fromSDLEvent (SDLUserEvent n) = #{const SDL_USEREVENT} + n
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
toSDLEvent n 
    | n >= #{const SDL_USEREVENT} &&
      n <  #{const SDL_NUMEVENTS} = SDLUserEvent (n - #{const SDL_USEREVENT})
toSDLEvent _ = error "Graphics.UI.SDL.Events.toSDLEvent: bad argument"

-- |High level event structure.
data Event
    = NoEvent
    | GotFocus [Focus]
    | LostFocus [Focus]
    | KeyDown !Keysym
    | KeyUp !Keysym
    | MouseMotion !Word16 !Word16 !Int16 !Int16
    | MouseButtonDown !Word16
                      !Word16
                      !MouseButton
    | MouseButtonUp !Word16
                    !Word16
                    !MouseButton
    | JoyAxisMotion !Word8 !Word8 !Int16
      -- ^ device index, axis index, axis value.
    | JoyBallMotion !Word8 !Word8 !Int16 !Int16
      -- ^ device index, trackball index, relative motion.
    | JoyHatMotion !Word8 !Word8 !Word8
      -- ^ device index, hat index, hat position.
    | JoyButtonDown !Word8 !Word8
      -- ^ device index, button index.
    | JoyButtonUp !Word8 !Word8
      -- ^ device index, button index.
    | VideoResize !Int !Int
      -- ^ When @Resizable@ is passed as a flag to 'Graphics.UI.SDL.Video.setVideoMode' the user is
      --   allowed to resize the applications window. When the window is resized
      --   an @VideoResize@ is reported, with the new window width and height values.
      --   When an @VideoResize@ is recieved the window should be resized to the
      --   new dimensions using 'Graphics.UI.SDL.Video.setVideoMode'.
    | VideoExpose
      -- ^ A @VideoExpose@ event is triggered when the screen has been modified
      --   outside of the application, usually by the window manager and needs to be redrawn.
    | Quit
    | User !UserEventID !Int !(Ptr ()) !(Ptr ())
    | Unknown
      deriving (Show,Eq)

data MouseButton
    = ButtonLeft
    | ButtonMiddle
    | ButtonRight
    | ButtonWheelUp
    | ButtonWheelDown
      deriving (Show,Eq,Ord)

instance Enum MouseButton Word8 where
    toEnum #{const SDL_BUTTON_LEFT} = ButtonLeft
    toEnum #{const SDL_BUTTON_MIDDLE} = ButtonMiddle
    toEnum #{const SDL_BUTTON_RIGHT} = ButtonRight
    toEnum #{const SDL_BUTTON_WHEELUP} = ButtonWheelUp
    toEnum #{const SDL_BUTTON_WHEELDOWN} = ButtonWheelDown
    toEnum _ = error "Graphics.UI.SDL.Events.toEnum: bad argument"
    fromEnum ButtonLeft = #{const SDL_BUTTON_LEFT}
    fromEnum ButtonMiddle = #{const SDL_BUTTON_MIDDLE}
    fromEnum ButtonRight = #{const SDL_BUTTON_RIGHT}
    fromEnum ButtonWheelUp = #{const SDL_BUTTON_WHEELUP}
    fromEnum ButtonWheelDown = #{const SDL_BUTTON_WHEELDOWN}
    succ ButtonLeft = ButtonMiddle
    succ ButtonMiddle = ButtonRight
    succ ButtonRight = ButtonWheelUp
    succ ButtonWheelUp = ButtonWheelDown
    succ _ = error "Graphics.UI.SDL.Events.succ: bad argument"
    pred ButtonMiddle = ButtonLeft
    pred ButtonRight = ButtonMiddle
    pred ButtonWheelUp = ButtonRight
    pred ButtonWheelDown = ButtonWheelUp
    pred _ = error "Graphics.UI.SDL.Events.pred: bad argument"
    enumFromTo x y | x > y = []
                   | x == y = [y]
                   | True = x : enumFromTo (succ x) y


data Focus
    = MouseFocus
    | InputFocus
    | ApplicationFocus
      deriving (Show,Eq,Ord)

instance Bounded Focus where
    minBound = MouseFocus
    maxBound = ApplicationFocus

instance Enum Focus Word8 where
    fromEnum MouseFocus = #{const SDL_APPMOUSEFOCUS}
    fromEnum InputFocus = #{const SDL_APPINPUTFOCUS}
    fromEnum ApplicationFocus = #{const SDL_APPACTIVE}
    toEnum #{const SDL_APPMOUSEFOCUS} = MouseFocus
    toEnum #{const SDL_APPINPUTFOCUS} = InputFocus
    toEnum #{const SDL_APPACTIVE} = ApplicationFocus
    toEnum _ = error "Graphics.UI.SDL.Events.toEnum: bad argument"
    succ MouseFocus = InputFocus
    succ InputFocus = ApplicationFocus
    succ _ = error "Graphics.UI.SDL.Events.succ: bad argument"
    pred InputFocus = MouseFocus
    pred ApplicationFocus = InputFocus
    pred _ = error "Graphics.UI.SDL.Events.pred: bad argument"
    enumFromTo x y | x > y = []
                   | x == y = [y]
                   | True = x : enumFromTo (succ x) y

-- |Typed user events ranging from 0 to 7
data UserEventID
    = UID0 | UID1 | UID2 | UID3 | UID4 | UID5 | UID6 | UID7
      deriving (Show,Eq,Prelude.Enum)

-- |A safe pointer keeps the type of the object it was created from
--  and checks it when it's deconstructed.
type SafePtr = Ptr ()

-- |Constructs a safe pointer from an arbitrary value.
toSafePtr :: (Typeable a) => a -> IO SafePtr
toSafePtr val
    = do stablePtr <- newStablePtr (typeOf val,val)
         return (castStablePtrToPtr stablePtr)

-- |Return the type of the object the safe pointer was created from.
typeOfSafePtr :: SafePtr -> IO TypeRep
typeOfSafePtr ptr
    = fmap fst (deRefStablePtr (castPtrToStablePtr ptr))

-- |Get object from a safe pointer. @Nothing@ on type mismatch.
tryFromSafePtr :: (Typeable a) => SafePtr -> IO (Maybe a)
tryFromSafePtr ptr
    = do (ty,val) <- deRefStablePtr (castPtrToStablePtr ptr)
         if ty == typeOf val
            then return (Just val)
            else return Nothing

-- |Get object from a safe pointer. Throws an exception on type mismatch.
fromSafePtr :: (Typeable a) => SafePtr -> IO a
fromSafePtr ptr
    = do ret <- tryFromSafePtr ptr
         case ret of
           Nothing -> error "Graphics.UI.SDL.Events.fromSafePtr: invalid type."
           Just a  -> return a

toEventType :: UserEventID -> Word8
toEventType eid = fromIntegral (Prelude.fromEnum eid)

fromEventType :: Word8 -> UserEventID
fromEventType etype = Prelude.toEnum (fromIntegral etype)

peekActiveEvent :: Ptr Event -> IO Event
peekActiveEvent ptr
    = do gain <- fmap toBool ((#{peek SDL_ActiveEvent, gain} ptr) :: IO Word8)
         state <- #{peek SDL_ActiveEvent, state} ptr :: IO Word8
         return $! (if gain then GotFocus else LostFocus) (fromBitmask state)

peekKey :: (Keysym -> Event) -> Ptr Event -> IO Event
peekKey mkEvent ptr
    = do keysym <- #{peek SDL_KeyboardEvent, keysym} ptr
         return $! mkEvent keysym

peekMouseMotion :: Ptr Event -> IO Event
peekMouseMotion ptr
    = do x <- #{peek SDL_MouseMotionEvent, x} ptr
         y <- #{peek SDL_MouseMotionEvent, y} ptr
         xrel <- #{peek SDL_MouseMotionEvent, xrel} ptr
         yrel <- #{peek SDL_MouseMotionEvent, yrel} ptr
         return $! MouseMotion x y xrel yrel

peekMouse :: (Word16 -> Word16 -> MouseButton -> Event) -> Ptr Event -> IO Event
peekMouse mkEvent ptr
    = do b <- #{peek SDL_MouseButtonEvent, button} ptr
         x <- #{peek SDL_MouseButtonEvent, x} ptr
         y <- #{peek SDL_MouseButtonEvent, y} ptr
         return $! mkEvent x y (toEnum (b::Word8))

peekJoyAxisMotion :: Ptr Event -> IO Event
peekJoyAxisMotion ptr
    = do which <- #{peek SDL_JoyAxisEvent, which} ptr
         axis <- #{peek SDL_JoyAxisEvent, axis} ptr
         value <- #{peek SDL_JoyAxisEvent, value} ptr
         return $! JoyAxisMotion which axis value

peekJoyBallMotion :: Ptr Event -> IO Event
peekJoyBallMotion ptr
    = do which <- #{peek SDL_JoyBallEvent, which} ptr
         ball <- #{peek SDL_JoyBallEvent, ball} ptr
         xrel <- #{peek SDL_JoyBallEvent, xrel} ptr
         yrel <- #{peek SDL_JoyBallEvent, yrel} ptr
         return $! JoyBallMotion which ball xrel yrel

peekJoyHatMotion :: Ptr Event -> IO Event
peekJoyHatMotion ptr
    = do which <- #{peek SDL_JoyHatEvent, which} ptr
         hat <- #{peek SDL_JoyHatEvent, hat} ptr
         value <- #{peek SDL_JoyHatEvent, value} ptr
         return $! JoyHatMotion which hat value

peekJoyButton :: (Word8 -> Word8 -> Event) -> Ptr Event -> IO Event
peekJoyButton mkEvent ptr
    = do which <- #{peek SDL_JoyButtonEvent, which} ptr
         button <- #{peek SDL_JoyButtonEvent, button} ptr
         return $! mkEvent which button

peekResize :: Ptr Event -> IO Event
peekResize ptr
    = do w <- #{peek SDL_ResizeEvent, w} ptr
         h <- #{peek SDL_ResizeEvent, h} ptr
         return $! VideoResize w h

peekUserEvent :: Ptr Event -> Word8 -> IO Event
peekUserEvent ptr n
    = do code <- #{peek SDL_UserEvent, code} ptr
         data1 <- #{peek SDL_UserEvent, data1} ptr
         data2 <- #{peek SDL_UserEvent, data2} ptr
         return $ User (fromEventType n) code data1 data2

getEventType :: Event -> Word8
getEventType = fromSDLEvent . eventToSDLEvent

eventToSDLEvent :: Event -> SDLEvent
eventToSDLEvent NoEvent = SDLNoEvent
eventToSDLEvent (GotFocus _) = SDLActiveEvent
eventToSDLEvent (LostFocus _) = SDLActiveEvent
eventToSDLEvent (KeyDown _) = SDLKeyDown
eventToSDLEvent (KeyUp _) = SDLKeyUp
eventToSDLEvent (MouseMotion _ _ _ _) = SDLMouseMotion
eventToSDLEvent (MouseButtonDown _ _ _) = SDLMouseButtonDown
eventToSDLEvent (MouseButtonUp _ _ _) = SDLMouseButtonUp
eventToSDLEvent (JoyAxisMotion _ _ _) = SDLJoyAxisMotion
eventToSDLEvent (JoyBallMotion _ _ _ _) = SDLJoyBallMotion
eventToSDLEvent (JoyHatMotion _ _ _) = SDLJoyHatMotion
eventToSDLEvent (JoyButtonDown _ _) = SDLJoyButtonDown
eventToSDLEvent (JoyButtonUp _ _) = SDLJoyButtonUp
eventToSDLEvent Quit = SDLQuit
eventToSDLEvent (VideoResize _ _) = SDLVideoResize
eventToSDLEvent VideoExpose = SDLVideoExpose
eventToSDLEvent (User uid _ _ _) = SDLUserEvent (toEventType uid)
eventToSDLEvent _ = error "Graphics.UI.SDL.Events.eventToSDLEvent: bad argument"

pokeActiveEvent :: Ptr Event -> Word8 -> [Focus] -> IO ()
pokeActiveEvent ptr gain focus
    = do #{poke SDL_ActiveEvent, gain} ptr gain
         #{poke SDL_ActiveEvent, state} ptr (toBitmask focus)

pokeKey :: Ptr Event -> Word8 -> Keysym -> IO ()
pokeKey ptr state keysym
    = do #{poke SDL_KeyboardEvent, state} ptr state
         #{poke SDL_KeyboardEvent, keysym} ptr keysym

pokeMouseMotion :: Ptr Event -> Word16 -> Word16 -> Int16 -> Int16 -> IO ()
pokeMouseMotion ptr x y xrel yrel
    = do #{poke SDL_MouseMotionEvent, x} ptr x
         #{poke SDL_MouseMotionEvent, y} ptr y
         #{poke SDL_MouseMotionEvent, xrel} ptr xrel
         #{poke SDL_MouseMotionEvent, yrel} ptr yrel

pokeMouseButton :: Ptr Event -> Word8 -> Word16 -> Word16 -> MouseButton -> IO ()
pokeMouseButton ptr state x y b
    = do #{poke SDL_MouseButtonEvent, x} ptr x
         #{poke SDL_MouseButtonEvent, y} ptr y
         #{poke SDL_MouseButtonEvent, state} ptr state
         #{poke SDL_MouseButtonEvent, button} ptr (fromEnum b)

pokeJoyAxisMotion :: Ptr Event -> Word8 -> Word8 -> Int16 -> IO ()
pokeJoyAxisMotion ptr which axis value
    = do #{poke SDL_JoyAxisEvent, which} ptr which
         #{poke SDL_JoyAxisEvent, axis} ptr axis
         #{poke SDL_JoyAxisEvent, value} ptr value

pokeJoyBallMotion :: Ptr Event -> Word8 -> Word8 -> Int16 -> Int16 -> IO ()
pokeJoyBallMotion ptr which ball xrel yrel
    = do #{poke SDL_JoyBallEvent, which} ptr which
         #{poke SDL_JoyBallEvent, ball} ptr ball
         #{poke SDL_JoyBallEvent, xrel} ptr xrel
         #{poke SDL_JoyBallEvent, yrel} ptr yrel

pokeJoyHatMotion :: Ptr Event -> Word8 -> Word8 -> Word8 -> IO ()
pokeJoyHatMotion ptr which hat value
    = do #{poke SDL_JoyHatEvent, which} ptr which
         #{poke SDL_JoyHatEvent, hat} ptr hat
         #{poke SDL_JoyHatEvent, value} ptr value

pokeJoyButton :: Ptr Event -> Word8 -> Word8 -> Word8 -> IO ()
pokeJoyButton ptr which button state
    = do #{poke SDL_JoyButtonEvent, which} ptr which
         #{poke SDL_JoyButtonEvent, button} ptr button
         #{poke SDL_JoyButtonEvent, state} ptr state

pokeResize :: Ptr Event -> Int -> Int -> IO ()
pokeResize ptr w h
    = do #{poke SDL_ResizeEvent, w} ptr w
         #{poke SDL_ResizeEvent, h} ptr h

pokeUserEvent :: Ptr Event -> UserEventID -> Int -> Ptr () -> Ptr () -> IO ()
pokeUserEvent ptr _eventId code data1 data2
    = do #{poke SDL_UserEvent, code} ptr code
         #{poke SDL_UserEvent, data1} ptr data1
         #{poke SDL_UserEvent, data2} ptr data2

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
               MouseMotion x y xrel yrel -> pokeMouseMotion ptr x y xrel yrel
               MouseButtonDown x y b -> pokeMouseButton ptr #{const SDL_PRESSED} x y b
               MouseButtonUp x y b   -> pokeMouseButton ptr #{const SDL_RELEASED} x y b
               JoyAxisMotion w a v   -> pokeJoyAxisMotion ptr w a v
               JoyBallMotion w b x y -> pokeJoyBallMotion ptr w b x y
               JoyHatMotion w h v    -> pokeJoyHatMotion ptr w h v
               JoyButtonDown w b     -> pokeJoyButton ptr w b #{const SDL_PRESSED}
               JoyButtonUp w b       -> pokeJoyButton ptr w b #{const SDL_RELEASED}
               Quit                  -> return ()
               VideoResize w h       -> pokeResize ptr w h
               VideoExpose           -> return ()
               User eventId c d1 d2  -> pokeUserEvent ptr eventId c d1 d2
               e                     -> failWithError $ "Unhandled eventtype: " ++ show e
    peek ptr
        = do eventType <- peekByteOff ptr 0
             case toSDLEvent eventType of
               SDLNoEvent         -> return NoEvent
               SDLActiveEvent     -> peekActiveEvent ptr
               SDLKeyDown         -> peekKey KeyDown ptr
               SDLKeyUp           -> peekKey KeyUp ptr
               SDLMouseMotion     -> peekMouseMotion ptr
               SDLMouseButtonDown -> peekMouse MouseButtonDown ptr
               SDLMouseButtonUp   -> peekMouse MouseButtonUp ptr
               SDLJoyAxisMotion   -> peekJoyAxisMotion ptr
               SDLJoyBallMotion   -> peekJoyBallMotion ptr
               SDLJoyHatMotion    -> peekJoyHatMotion ptr
               SDLJoyButtonDown   -> peekJoyButton JoyButtonDown ptr
               SDLJoyButtonUp     -> peekJoyButton JoyButtonUp ptr
               SDLQuit            -> return Quit
--           SDLSysWMEvent
               SDLVideoResize     -> peekResize ptr
               SDLVideoExpose     -> return VideoExpose
               SDLUserEvent n     -> peekUserEvent ptr n
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
foreign import ccall unsafe "SDL_GetKeyName" sdlGetKeyName :: #{type SDLKey} -> IO CString

-- | Gets the name of an SDL virtual keysym.
getKeyName :: SDLKey -> String
getKeyName key = unsafePerformIO $
                 sdlGetKeyName (fromEnum key) >>= peekCString

-- SDLMod SDL_GetModState(void);
foreign import ccall unsafe "SDL_GetModState" sdlGetModState :: IO #{type SDLMod}

-- | Gets the state of modifier keys.
getModState :: IO [Modifier]
getModState = fmap fromBitmask sdlGetModState

-- void SDL_SetModState(SDLMod modstate);
foreign import ccall unsafe "SDL_SetModState" sdlSetModState :: #{type SDLMod} -> IO ()

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
-- | Pumps the event loop, gathering events from the input devices.
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
    = loop
    where loop = do pumpEvents
                    event <- pollEvent
                    case event of
                      NoEvent -> threadDelay 10 >> loop
                      _ -> return event

-- | Waits indefinitely for the next available event. Blocks Haskell threads.
waitEventBlocking :: IO Event
waitEventBlocking
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

-- |Enable or disable events from being processed.
enableEvent :: SDLEvent -> Bool -> IO ()
enableEvent event on
    = sdlEventState (fromSDLEvent event) (fromToggle state) >> return ()
    where state
              | on = Enable
              | otherwise = Disable

-- |Checks current state of a event. See also 'enableEvent'.
queryEventState :: SDLEvent -> IO Bool
queryEventState event
    = fmap (==1) (sdlEventState (fromSDLEvent event) (fromToggle Query))

-- Uint8 SDL_GetAppState(void);
foreign import ccall unsafe "SDL_GetAppState" sdlGetAppState :: IO Word8

-- | Gets the state of the application.
getAppState :: IO [Focus]
getAppState = fmap fromBitmask sdlGetAppState

