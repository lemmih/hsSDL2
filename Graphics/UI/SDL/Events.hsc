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
module Graphics.UI.SDL.Events where

import Control.Applicative
import Control.Monad ((>=>), void)
import Data.Word
import Foreign hiding (void)
import Foreign.C
import Graphics.UI.SDL.Keysym
import Graphics.UI.SDL.Types (Position, Size, mkPosition, mkSize)

data Event = Event { eventTimestamp :: Word32, eventData :: EventData }
  deriving (Eq, Show)

data MouseButton = LeftButton | RightButton | MiddleButton | MouseX1 | MouseX2
  deriving (Eq, Show)

data EventData
  = Keyboard { keyMovement :: KeyMovement
             , keyWindowID :: Word32
             , keyRepeat :: Bool
             , keySym :: Keysym
             }
  | Window { windowID :: Word32
           , windowEvent :: WindowEvent
           }
  | TextEditing -- TODO
  | TextInput { textInputWindowID :: Word32
              , textInput :: String
              }
  | MouseMotion { mouseMotionWindowID :: Word32
                , mouseMotionMouseID :: Word32
                , mouseMotionState :: Word32 -- TODO Set of possible modifiers
                , mouseMotionPosition :: Position
                , mouseMotionXRelMotion :: Int32
                , mouseMotionYRelMotion :: Int32 -- TODO Tuple up?
                }
  | MouseButton { mouseButtonWindowID :: Word32
                , mouseButtonMouseID :: Word32
                , mouseButtton :: MouseButton
                , mouseButtonState :: Word8 -- TODO See MouseMotion
                , mouseButtonAt :: Position
                }
  | MouseWheel { mouseWheelWindowID :: Word32
               , mouseWheelMouseID :: Word32
               , mouseWheelHorizontalScroll :: Int32
               , mouseWheelVerticalScroll :: Int32
               }
  | JoyAxis -- TODO
  | JoyBall -- TODO
  | JoyHat -- TODO
  | JoyButton -- TODO
  | JoyDevice -- TODO
  | ControllerAxis -- TODO
  | ControllerButton -- TODO
  | ControllerDevice -- TODO
  | TouchFinger { touchFingerType :: Word32
                , touchTimestamp :: Word32
                , touchID :: CLong
                , touchFingerID :: CLong
                , touchX :: CFloat
                , touchY :: CFloat
                , touchDx :: CFloat
                , touchDy :: CFloat
                , touchPressure :: CFloat
                }
  | MultiGesture -- TODO
  | DollarGesture -- TODO
  | Drop -- TODO
  | Quit
  deriving (Eq, Show)

data WindowEvent
  = Shown
  | Hidden
  | Exposed
  | Moved { windowMovedTo :: Position }
  | Resized { windowResizedTo :: Size }
  | SizeChanged
  | Minimized
  | Maximized
  | Restored
  | GainedMouseFocus
  | LostMouseFocus
  | GainedKeyboardFocus
  | LostKeyboardFocus
  | Closing
  deriving (Eq, Show)

data KeyMovement = KeyUp | KeyDown
  deriving (Eq, Show)

instance Storable Event where
  sizeOf = const #{size SDL_Event}

  alignment = const 4

  poke ptr (Event timestamp body) = do
    #{poke SDL_CommonEvent, type} ptr (sdlEventType body)
    #{poke SDL_CommonEvent, timestamp} ptr timestamp

    case body of
      Keyboard m w r s -> do
        #{poke SDL_KeyboardEvent, windowID} ptr w
        #{poke SDL_KeyboardEvent, state} ptr (sdlKeyState m)
        #{poke SDL_KeyboardEvent, repeat} ptr
          (if r then 1 else 0 :: Word8)
        #{poke SDL_KeyboardEvent, padding2} ptr padding8
        #{poke SDL_KeyboardEvent, padding3} ptr padding8
        #{poke SDL_KeyboardEvent, keysym} ptr s

   where padding8 = 0 :: Word8

  peek ptr = do
    evType <- #{peek SDL_CommonEvent, type} ptr
    Event <$> #{peek SDL_CommonEvent, timestamp} ptr <*> peekEvent evType

   where

    peekEvent :: Word32 -> IO EventData
    peekEvent e
      | isKeyboard e =
          Keyboard <$> case e of
                        #{const SDL_KEYDOWN} -> pure KeyDown
                        #{const SDL_KEYUP} -> pure KeyUp
                        _ -> error "Unknown key movement when parsing SDL_KeybordEvent"
                   <*> #{peek SDL_KeyboardEvent, windowID} ptr
                   <*> (uint8Bool <$> #{peek SDL_KeyboardEvent, repeat} ptr)
                   <*> #{peek SDL_KeyboardEvent, keysym} ptr

      | isWindow e =
          Window <$> #{peek SDL_WindowEvent, windowID} ptr
                 <*> (#{peek SDL_WindowEvent, event} ptr >>= peekWindowEvent)

      | isTextInput e =
          TextInput <$> #{peek SDL_TextInputEvent, windowID} ptr
                    <*> peekCString (ptr `plusPtr` #{offset SDL_TextInputEvent, text})

      | isTextEditing e = pure TextEditing -- TODO

      | isMouseMotion e =
          MouseMotion <$> #{peek SDL_MouseMotionEvent, windowID} ptr
                      <*> #{peek SDL_MouseMotionEvent, which} ptr
                      <*> #{peek SDL_MouseMotionEvent, state} ptr
                      <*> (mkPosition <$> #{peek SDL_MouseMotionEvent, x} ptr
                                      <*> #{peek SDL_MouseMotionEvent, y} ptr)
                      <*> #{peek SDL_MouseMotionEvent, xrel} ptr
                      <*> #{peek SDL_MouseMotionEvent, yrel} ptr

      | isMouseButton e =
          MouseButton <$> #{peek SDL_MouseButtonEvent, windowID} ptr
                      <*> #{peek SDL_MouseButtonEvent, which} ptr
                      <*> (sdlMouseButton <$> #{peek SDL_MouseButtonEvent, button} ptr)
                      <*> #{peek SDL_MouseButtonEvent, state} ptr
                      <*> (mkPosition <$> #{peek SDL_MouseButtonEvent, x} ptr
                                      <*> #{peek SDL_MouseButtonEvent, y} ptr)

      | isMouseWheel e =
          MouseWheel <$> #{peek SDL_MouseWheelEvent, windowID} ptr
                     <*> #{peek SDL_MouseWheelEvent, which} ptr
                     <*> #{peek SDL_MouseWheelEvent, x} ptr
                     <*> #{peek SDL_MouseWheelEvent, y} ptr

      | isJoyAxis e = pure JoyAxis
      | isJoyBall e = pure JoyBall
      | isJoyHat e = pure JoyHat
      | isJoyButton e = pure JoyButton
      | isJoyDevice e = pure JoyDevice
      | isControllerAxis e = pure ControllerAxis
      | isControllerButton e = pure ControllerButton
      | isTouchFinger e = 
          TouchFinger <$> #{peek SDL_TouchFingerEvent, type} ptr
                      <*> #{peek SDL_TouchFingerEvent, timestamp} ptr
                      <*> #{peek SDL_TouchFingerEvent, touchId} ptr
                      <*> #{peek SDL_TouchFingerEvent, fingerId} ptr
                      <*> #{peek SDL_TouchFingerEvent, x} ptr
                      <*> #{peek SDL_TouchFingerEvent, y} ptr
                      <*> #{peek SDL_TouchFingerEvent, dx} ptr
                      <*> #{peek SDL_TouchFingerEvent, dy} ptr
                      <*> #{peek SDL_TouchFingerEvent, pressure} ptr
      
      | isMultiGesture e = pure MultiGesture
      | isDollarGesture e = pure DollarGesture
      | isDrop e = pure Drop
      | isQuit e = pure Quit

      | otherwise = error $ "Unknown event type: " ++ show e

    peekWindowEvent :: Word8 -> IO WindowEvent
    peekWindowEvent e = case e of
      #{const SDL_WINDOWEVENT_SHOWN} -> pure Shown
      #{const SDL_WINDOWEVENT_HIDDEN} -> pure Hidden
      #{const SDL_WINDOWEVENT_EXPOSED} -> pure Exposed
      #{const SDL_WINDOWEVENT_MOVED} ->
        Moved <$> (mkPosition <$> #{peek SDL_WindowEvent, data1} ptr
                              <*> #{peek SDL_WindowEvent, data2} ptr)
      #{const SDL_WINDOWEVENT_RESIZED} ->
        Resized <$> (mkSize <$> #{peek SDL_WindowEvent, data1} ptr
                            <*> #{peek SDL_WindowEvent, data2} ptr)
      #{const SDL_WINDOWEVENT_SIZE_CHANGED} -> pure SizeChanged
      #{const SDL_WINDOWEVENT_MINIMIZED} -> pure Minimized
      #{const SDL_WINDOWEVENT_MAXIMIZED} -> pure Maximized
      #{const SDL_WINDOWEVENT_RESTORED} -> pure Restored
      #{const SDL_WINDOWEVENT_ENTER} -> pure GainedMouseFocus
      #{const SDL_WINDOWEVENT_LEAVE} -> pure LostMouseFocus
      #{const SDL_WINDOWEVENT_FOCUS_GAINED} -> pure GainedKeyboardFocus
      #{const SDL_WINDOWEVENT_FOCUS_LOST} -> pure LostKeyboardFocus
      #{const SDL_WINDOWEVENT_CLOSE} -> pure Closing
      unknown -> error $ "Unknown SDL_WINDOWEVENT: " ++ show unknown

    isKeyboard = (`elem` [ #{const SDL_KEYUP}, #{ const SDL_KEYDOWN } ])
    isWindow = (== #{const SDL_WINDOWEVENT})
    isTextInput = (== #{const SDL_TEXTINPUT})
    isTextEditing = (== #{const SDL_TEXTEDITING})
    isMouseMotion = (== #{const SDL_MOUSEMOTION})
    isMouseButton = (`elem` [#{const SDL_MOUSEBUTTONDOWN}, #{const SDL_MOUSEBUTTONUP}])
    isMouseWheel = (== #{const SDL_MOUSEWHEEL})
    isJoyAxis = (== #{const SDL_JOYAXISMOTION})
    isJoyBall = (== #{const SDL_JOYBALLMOTION})
    isJoyHat = (== #{const SDL_JOYHATMOTION})
    isJoyButton = (`elem` [#{const SDL_JOYBUTTONDOWN}, #{const SDL_JOYBUTTONUP}])
    isJoyDevice = (`elem` [#{const SDL_JOYDEVICEADDED}, #{const SDL_JOYDEVICEREMOVED}])
    isControllerAxis = (== #{const SDL_CONTROLLERAXISMOTION})
    isControllerButton = (`elem` [#{const SDL_CONTROLLERBUTTONDOWN}, #{const SDL_CONTROLLERBUTTONUP}])
    isControllerDevice = (`elem` [#{const SDL_CONTROLLERDEVICEADDED}, #{const SDL_CONTROLLERDEVICEREMOVED}])
    isTouchFinger = (`elem` [ #{const SDL_FINGERMOTION}, #{const SDL_FINGERDOWN}, #{const SDL_FINGERUP}])
    isMultiGesture = (== #{const SDL_MULTIGESTURE})
    isDollarGesture = (== #{const SDL_DOLLARGESTURE})
    isDrop = (== #{const SDL_DROPFILE})
    isQuit = (== #{const SDL_QUIT})

    uint8Bool :: Word8 -> Bool
    uint8Bool = (== 0)

    sdlMouseButton :: Word8 -> MouseButton
    sdlMouseButton #{const SDL_BUTTON_LEFT} = LeftButton
    sdlMouseButton #{const SDL_BUTTON_MIDDLE} = MiddleButton
    sdlMouseButton #{const SDL_BUTTON_RIGHT} = RightButton
    sdlMouseButton #{const SDL_BUTTON_X1} = MouseX1
    sdlMouseButton #{const SDL_BUTTON_X2} = MouseX2

sdlEventType :: EventData -> Word32
sdlEventType (Keyboard KeyUp _ _ _) = #{const SDL_KEYUP}
sdlEventType (Keyboard KeyDown _ _ _) = #{const SDL_KEYDOWN}

sdlKeyState :: KeyMovement -> Word8
sdlKeyState KeyUp = #{const SDL_RELEASED}
sdlKeyState KeyDown = #{const SDL_PRESSED}

foreign import ccall "SDL_PollEvent" sdlPollEvent :: Ptr Event -> IO Int

-- | Polls for currently pending events.
pollEvent :: IO (Maybe Event)
pollEvent = alloca $ \ptr -> do
  ret <- sdlPollEvent ptr
  case ret of
    0 -> return Nothing
    _ -> maybePeek peek ptr

foreign import ccall "wrapper"
  mkEventFilter :: (Ptr () -> Ptr Event -> IO ()) -> IO (FunPtr (Ptr () -> Ptr Event -> IO ()))

foreign import ccall "SDL_AddEventWatch"
  sdlAddEventWatch :: FunPtr (Ptr () -> Ptr Event -> IO ()) -> Ptr () -> IO ()

-- TODO Adding an event watch seems to stop ^C terminating the program
addEventWatch :: (Event -> IO a) -> IO ()
addEventWatch callback = do
  cb <- mkEventFilter $ \_ -> peek >=> void . callback
  sdlAddEventWatch cb nullPtr
