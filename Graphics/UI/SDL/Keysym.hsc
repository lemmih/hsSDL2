{-# LANGUAGE GeneralizedNewtypeDeriving #-}
#include "SDL.h"
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.SDL.Keysym
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Graphics.UI.SDL.Keysym where

import Prelude hiding (Either(Left, Right))
import Control.Applicative
import Foreign
import Graphics.UI.SDL.Keycode (Keycode, sdlKeycode)

data Keysym = Keysym { keyScancode :: Scancode
                     , keyKeycode :: Keycode
                     , keyModifiers :: Word16
                     }
  deriving (Eq, Show)

instance Storable Keysym where
  sizeOf = const #{size SDL_Keysym}

  alignment = const 4

  poke ptr (Keysym s k m) = do
    -- TODO? Do we care about poking keysyms?
    -- #{poke SDL_Keysym, scancode} ptr s
    -- #{poke SDL_Keysym, sym} ptr k
    #{poke SDL_Keysym, mod} ptr m
    #{poke SDL_Keysym, unused} ptr (0 :: Word32)

  peek ptr = Keysym <$> (sdlScanCode <$> #{peek SDL_Keysym, scancode} ptr)
                    <*> (sdlKeycode <$> #{peek SDL_Keysym, sym} ptr)
                    <*> #{peek SDL_Keysym, mod} ptr

data Scancode
  = A
  | B
  | C
  | D
  | E
  | F
  | G
  | H
  | I
  | J
  | K
  | L
  | M
  | N
  | O
  | P
  | Q
  | R
  | S
  | T
  | U
  | V
  | W
  | X
  | Y
  | Z
  | Number1
  | Number2
  | Number3
  | Number4
  | Number5
  | Number6
  | Number7
  | Number8
  | Number9
  | Number0
  | Return
  | Escape
  | Backspace
  | Tab
  | Space
  | Minus
  | Equals
  | LeftBracket
  | RightBracket
  | Backslash
  | NonUSHash
  | Semicolon
  | Apostrophe
  | Grave
  | Comma
  | Period
  | Slash
  | Capslock
  | F1
  | F2
  | F3
  | F4
  | F5
  | F6
  | F7
  | F8
  | F9
  | F10
  | F11
  | F12
  | PrintScreen
  | ScrollLock
  | Pause
  | Insert
  | Home
  | PageUp
  | Delete
  | End
  | PageDown
  | Right
  | Left
  | Down
  | Up
  | NumLockClear
  | KeypadDivide
  | KeypadMultiply
  | KeypadMinus
  | KeypadPlus
  | KeypadEnter
  | Keypad1
  | Keypad2
  | Keypad3
  | Keypad4
  | Keypad5
  | Keypad6
  | Keypad7
  | Keypad8
  | Keypad9
  | Keypad0
  | KeypadPeriod
  | NonUSBackslash
  | Application
  | Power
  | KeypadEquals
  | F13
  | F14
  | F15
  | F16
  | F17
  | F18
  | F19
  | F20
  | F21
  | F22
  | F23
  | F24
  | Execute
  | Help
  | Menu
  | Select
  | Stop
  | Again
  | Undo
  | Cut
  | Copy
  | Paste
  | Find
  | Mute
  | VolumeUp
  | VolumeDown
  | KeypadComma
  | KeyPadEqualsAs400
  | International1
  | International2
  | International3
  | International4
  | International5
  | International6
  | International7
  | International8
  | International9
  | Lang1
  | Lang2
  | Lang3
  | Lang4
  | Lang5
  | Lang6
  | Lang7
  | Lang8
  | Lang9
  | AltErase
  | SysReq
  | Cancel
  | Clear
  | Prior
  | Return2
  | Separator
  | Out
  | Oper
  | ClearAgain
  | CrSel
  | ExSel
  | Keypad00
  | Keypad000
  | ThousandSeparator
  | DecimalSeparator
  | CurrencyUnit
  | CurrencySubunit
  | KeypadLeftParen
  | KeypadRightParen
  | KeypadLeftBrace
  | KeypadRightBrace
  | KeypadTab
  | KeypadBackspace
  | KeypadA
  | KeypadB
  | KeypadC
  | KeypadD
  | KeypadE
  | KeypadF
  | KeypadXOR
  | KeypadPower
  | KeypadPercent
  | KeypadLess
  | KeypadGreater
  | KeypadAmpersand
  | KeypadDoubleAmpersand
  | KeypadVerticalBar
  | KeypadDoubleVerticalBar
  | KeypadColon
  | KeypadHash
  | KeypadSpace
  | KeypadAt
  | KeypadExclamation
  | KeypadMemStore
  | KeypadMemRecall
  | KeypadMemClear
  | KeypadMemAdd
  | KeypadMemSubstract
  | KeypadMemMultiply
  | KeypadMemDivide
  | KeypadPlusMinus
  | KeypadClear
  | KeypadClearEntry
  | KeypadBinary
  | KeypadOctal
  | KeypadDecimal
  | KeypadHexadecimal
  | LeftControl
  | LeftShift
  | LeftAlt
  | LeftGUI
  | RightControl
  | RightShift
  | RightAlt
  | RightGUI
  | Mode
  | AudioNext
  | AudioPrevious
  | AudioStop
  | AudioPlay
  | AudioMute
  | MediaSelect
  | WWW
  | Mail
  | Calculator
  | Computer
  | ACSearch
  | ACHome
  | ACBack
  | ACForward
  | ACStop
  | ACRefresh
  | ACBookmarks
  | BrightnessDown
  | BrightnessUp
  | DisplaySwitch
  | KBIllumToggle
  | KBIllumDown
  | KBIllumUp
  | Eject
  | Sleep
  | App1
  | App2
  deriving (Eq, Show)

sdlScanCode :: #{type SDL_Scancode} -> Scancode
sdlScanCode #{const SDL_SCANCODE_A} = A
sdlScanCode #{const SDL_SCANCODE_B} = B
sdlScanCode #{const SDL_SCANCODE_C} = C
sdlScanCode #{const SDL_SCANCODE_D} = D
sdlScanCode #{const SDL_SCANCODE_E} = E
sdlScanCode #{const SDL_SCANCODE_F} = F
sdlScanCode #{const SDL_SCANCODE_G} = G
sdlScanCode #{const SDL_SCANCODE_H} = H
sdlScanCode #{const SDL_SCANCODE_I} = I
sdlScanCode #{const SDL_SCANCODE_J} = J
sdlScanCode #{const SDL_SCANCODE_K} = K
sdlScanCode #{const SDL_SCANCODE_L} = L
sdlScanCode #{const SDL_SCANCODE_M} = M
sdlScanCode #{const SDL_SCANCODE_N} = N
sdlScanCode #{const SDL_SCANCODE_O} = O
sdlScanCode #{const SDL_SCANCODE_P} = P
sdlScanCode #{const SDL_SCANCODE_Q} = Q
sdlScanCode #{const SDL_SCANCODE_R} = R
sdlScanCode #{const SDL_SCANCODE_S} = S
sdlScanCode #{const SDL_SCANCODE_T} = T
sdlScanCode #{const SDL_SCANCODE_U} = U
sdlScanCode #{const SDL_SCANCODE_V} = V
sdlScanCode #{const SDL_SCANCODE_W} = W
sdlScanCode #{const SDL_SCANCODE_X} = X
sdlScanCode #{const SDL_SCANCODE_Y} = Y
sdlScanCode #{const SDL_SCANCODE_Z} = Z
sdlScanCode #{const SDL_SCANCODE_1} = Number1
sdlScanCode #{const SDL_SCANCODE_2} = Number2
sdlScanCode #{const SDL_SCANCODE_3} = Number3
sdlScanCode #{const SDL_SCANCODE_4} = Number4
sdlScanCode #{const SDL_SCANCODE_5} = Number5
sdlScanCode #{const SDL_SCANCODE_6} = Number6
sdlScanCode #{const SDL_SCANCODE_7} = Number7
sdlScanCode #{const SDL_SCANCODE_8} = Number8
sdlScanCode #{const SDL_SCANCODE_9} = Number9
sdlScanCode #{const SDL_SCANCODE_0} = Number0
sdlScanCode #{const SDL_SCANCODE_RETURN} = Return
sdlScanCode #{const SDL_SCANCODE_ESCAPE} = Escape
sdlScanCode #{const SDL_SCANCODE_BACKSPACE} = Backspace
sdlScanCode #{const SDL_SCANCODE_TAB} = Tab
sdlScanCode #{const SDL_SCANCODE_SPACE} = Space
sdlScanCode #{const SDL_SCANCODE_MINUS} = Minus
sdlScanCode #{const SDL_SCANCODE_EQUALS} = Equals
sdlScanCode #{const SDL_SCANCODE_LEFTBRACKET} = LeftBracket
sdlScanCode #{const SDL_SCANCODE_RIGHTBRACKET} = RightBracket
sdlScanCode #{const SDL_SCANCODE_BACKSLASH} = Backslash
sdlScanCode #{const SDL_SCANCODE_NONUSHASH} = NonUSHash
sdlScanCode #{const SDL_SCANCODE_SEMICOLON} = Semicolon
sdlScanCode #{const SDL_SCANCODE_APOSTROPHE} = Apostrophe
sdlScanCode #{const SDL_SCANCODE_GRAVE} = Grave
sdlScanCode #{const SDL_SCANCODE_COMMA} = Comma
sdlScanCode #{const SDL_SCANCODE_PERIOD} = Period
sdlScanCode #{const SDL_SCANCODE_SLASH} = Slash
sdlScanCode #{const SDL_SCANCODE_CAPSLOCK} = Capslock
sdlScanCode #{const SDL_SCANCODE_F1} = F1
sdlScanCode #{const SDL_SCANCODE_F2} = F2
sdlScanCode #{const SDL_SCANCODE_F3} = F3
sdlScanCode #{const SDL_SCANCODE_F4} = F4
sdlScanCode #{const SDL_SCANCODE_F5} = F5
sdlScanCode #{const SDL_SCANCODE_F6} = F6
sdlScanCode #{const SDL_SCANCODE_F7} = F7
sdlScanCode #{const SDL_SCANCODE_F8} = F8
sdlScanCode #{const SDL_SCANCODE_F9} = F9
sdlScanCode #{const SDL_SCANCODE_F10} = F10
sdlScanCode #{const SDL_SCANCODE_F11} = F11
sdlScanCode #{const SDL_SCANCODE_F12} = F12
sdlScanCode #{const SDL_SCANCODE_PRINTSCREEN} = PrintScreen
sdlScanCode #{const SDL_SCANCODE_SCROLLLOCK} = ScrollLock
sdlScanCode #{const SDL_SCANCODE_PAUSE} = Pause
sdlScanCode #{const SDL_SCANCODE_INSERT} = Insert
sdlScanCode #{const SDL_SCANCODE_HOME} = Home
sdlScanCode #{const SDL_SCANCODE_PAGEUP} = PageUp
sdlScanCode #{const SDL_SCANCODE_DELETE} = Delete
sdlScanCode #{const SDL_SCANCODE_END} = End
sdlScanCode #{const SDL_SCANCODE_PAGEDOWN} = PageDown
sdlScanCode #{const SDL_SCANCODE_RIGHT} = Right
sdlScanCode #{const SDL_SCANCODE_LEFT} = Left
sdlScanCode #{const SDL_SCANCODE_DOWN} = Down
sdlScanCode #{const SDL_SCANCODE_UP} = Up
sdlScanCode #{const SDL_SCANCODE_NUMLOCKCLEAR} = NumLockClear
sdlScanCode #{const SDL_SCANCODE_KP_DIVIDE} = KeypadDivide
sdlScanCode #{const SDL_SCANCODE_KP_MULTIPLY} = KeypadMultiply
sdlScanCode #{const SDL_SCANCODE_KP_MINUS} = KeypadMinus
sdlScanCode #{const SDL_SCANCODE_KP_PLUS} = KeypadPlus
sdlScanCode #{const SDL_SCANCODE_KP_ENTER} = KeypadEnter
sdlScanCode #{const SDL_SCANCODE_KP_1} = Keypad1
sdlScanCode #{const SDL_SCANCODE_KP_2} = Keypad2
sdlScanCode #{const SDL_SCANCODE_KP_3} = Keypad3
sdlScanCode #{const SDL_SCANCODE_KP_4} = Keypad4
sdlScanCode #{const SDL_SCANCODE_KP_5} = Keypad5
sdlScanCode #{const SDL_SCANCODE_KP_6} = Keypad6
sdlScanCode #{const SDL_SCANCODE_KP_7} = Keypad7
sdlScanCode #{const SDL_SCANCODE_KP_8} = Keypad8
sdlScanCode #{const SDL_SCANCODE_KP_9} = Keypad9
sdlScanCode #{const SDL_SCANCODE_KP_0} = Keypad0
sdlScanCode #{const SDL_SCANCODE_KP_PERIOD} = KeypadPeriod
sdlScanCode #{const SDL_SCANCODE_NONUSBACKSLASH} = NonUSBackslash
sdlScanCode #{const SDL_SCANCODE_APPLICATION} = Application
sdlScanCode #{const SDL_SCANCODE_POWER} = Power
sdlScanCode #{const SDL_SCANCODE_KP_EQUALS} = KeypadEquals
sdlScanCode #{const SDL_SCANCODE_F13} = F13
sdlScanCode #{const SDL_SCANCODE_F14} = F14
sdlScanCode #{const SDL_SCANCODE_F15} = F15
sdlScanCode #{const SDL_SCANCODE_F16} = F16
sdlScanCode #{const SDL_SCANCODE_F17} = F17
sdlScanCode #{const SDL_SCANCODE_F18} = F18
sdlScanCode #{const SDL_SCANCODE_F19} = F19
sdlScanCode #{const SDL_SCANCODE_F20} = F20
sdlScanCode #{const SDL_SCANCODE_F21} = F21
sdlScanCode #{const SDL_SCANCODE_F22} = F22
sdlScanCode #{const SDL_SCANCODE_F23} = F23
sdlScanCode #{const SDL_SCANCODE_F24} = F24
sdlScanCode #{const SDL_SCANCODE_EXECUTE} = Execute
sdlScanCode #{const SDL_SCANCODE_HELP} = Help
sdlScanCode #{const SDL_SCANCODE_MENU} = Menu
sdlScanCode #{const SDL_SCANCODE_SELECT} = Select
sdlScanCode #{const SDL_SCANCODE_STOP} = Stop
sdlScanCode #{const SDL_SCANCODE_AGAIN} = Again
sdlScanCode #{const SDL_SCANCODE_UNDO} = Undo
sdlScanCode #{const SDL_SCANCODE_CUT} = Cut
sdlScanCode #{const SDL_SCANCODE_COPY} = Copy
sdlScanCode #{const SDL_SCANCODE_PASTE} = Paste
sdlScanCode #{const SDL_SCANCODE_FIND} = Find
sdlScanCode #{const SDL_SCANCODE_MUTE} = Mute
sdlScanCode #{const SDL_SCANCODE_VOLUMEUP} = VolumeUp
sdlScanCode #{const SDL_SCANCODE_VOLUMEDOWN} = VolumeDown
sdlScanCode #{const SDL_SCANCODE_KP_COMMA} = KeypadComma
sdlScanCode #{const SDL_SCANCODE_KP_EQUALSAS400} = KeyPadEqualsAs400
sdlScanCode #{const SDL_SCANCODE_INTERNATIONAL1} = International1
sdlScanCode #{const SDL_SCANCODE_INTERNATIONAL2} = International2
sdlScanCode #{const SDL_SCANCODE_INTERNATIONAL3} = International3
sdlScanCode #{const SDL_SCANCODE_INTERNATIONAL4} = International4
sdlScanCode #{const SDL_SCANCODE_INTERNATIONAL5} = International5
sdlScanCode #{const SDL_SCANCODE_INTERNATIONAL6} = International6
sdlScanCode #{const SDL_SCANCODE_INTERNATIONAL7} = International7
sdlScanCode #{const SDL_SCANCODE_INTERNATIONAL8} = International8
sdlScanCode #{const SDL_SCANCODE_INTERNATIONAL9} = International9
sdlScanCode #{const SDL_SCANCODE_LANG1} = Lang1
sdlScanCode #{const SDL_SCANCODE_LANG2} = Lang2
sdlScanCode #{const SDL_SCANCODE_LANG3} = Lang3
sdlScanCode #{const SDL_SCANCODE_LANG4} = Lang4
sdlScanCode #{const SDL_SCANCODE_LANG5} = Lang5
sdlScanCode #{const SDL_SCANCODE_LANG6} = Lang6
sdlScanCode #{const SDL_SCANCODE_LANG7} = Lang7
sdlScanCode #{const SDL_SCANCODE_LANG8} = Lang8
sdlScanCode #{const SDL_SCANCODE_LANG9} = Lang9
sdlScanCode #{const SDL_SCANCODE_ALTERASE} = AltErase
sdlScanCode #{const SDL_SCANCODE_SYSREQ} = SysReq
sdlScanCode #{const SDL_SCANCODE_CANCEL} = Cancel
sdlScanCode #{const SDL_SCANCODE_CLEAR} = Clear
sdlScanCode #{const SDL_SCANCODE_PRIOR} = Prior
sdlScanCode #{const SDL_SCANCODE_RETURN2} = Return2
sdlScanCode #{const SDL_SCANCODE_SEPARATOR} = Separator
sdlScanCode #{const SDL_SCANCODE_OUT} = Out
sdlScanCode #{const SDL_SCANCODE_OPER} = Oper
sdlScanCode #{const SDL_SCANCODE_CLEARAGAIN} = ClearAgain
sdlScanCode #{const SDL_SCANCODE_CRSEL} = CrSel
sdlScanCode #{const SDL_SCANCODE_EXSEL} = ExSel
sdlScanCode #{const SDL_SCANCODE_KP_00} = Keypad00
sdlScanCode #{const SDL_SCANCODE_KP_000} = Keypad000
sdlScanCode #{const SDL_SCANCODE_THOUSANDSSEPARATOR} = ThousandSeparator
sdlScanCode #{const SDL_SCANCODE_DECIMALSEPARATOR} = DecimalSeparator
sdlScanCode #{const SDL_SCANCODE_CURRENCYUNIT} = CurrencyUnit
sdlScanCode #{const SDL_SCANCODE_CURRENCYSUBUNIT} = CurrencySubunit
sdlScanCode #{const SDL_SCANCODE_KP_LEFTPAREN} = KeypadLeftParen
sdlScanCode #{const SDL_SCANCODE_KP_RIGHTPAREN} = KeypadRightParen
sdlScanCode #{const SDL_SCANCODE_KP_LEFTBRACE} = KeypadLeftBrace
sdlScanCode #{const SDL_SCANCODE_KP_RIGHTBRACE} = KeypadRightBrace
sdlScanCode #{const SDL_SCANCODE_KP_TAB} = KeypadTab
sdlScanCode #{const SDL_SCANCODE_KP_BACKSPACE} = KeypadBackspace
sdlScanCode #{const SDL_SCANCODE_KP_A} = KeypadA
sdlScanCode #{const SDL_SCANCODE_KP_B} = KeypadB
sdlScanCode #{const SDL_SCANCODE_KP_C} = KeypadC
sdlScanCode #{const SDL_SCANCODE_KP_D} = KeypadD
sdlScanCode #{const SDL_SCANCODE_KP_E} = KeypadE
sdlScanCode #{const SDL_SCANCODE_KP_F} = KeypadF
sdlScanCode #{const SDL_SCANCODE_KP_XOR} = KeypadXOR
sdlScanCode #{const SDL_SCANCODE_KP_POWER} = KeypadPower
sdlScanCode #{const SDL_SCANCODE_KP_PERCENT} = KeypadPercent
sdlScanCode #{const SDL_SCANCODE_KP_LESS} = KeypadLess
sdlScanCode #{const SDL_SCANCODE_KP_GREATER} = KeypadGreater
sdlScanCode #{const SDL_SCANCODE_KP_AMPERSAND} = KeypadAmpersand
sdlScanCode #{const SDL_SCANCODE_KP_DBLAMPERSAND} = KeypadDoubleAmpersand
sdlScanCode #{const SDL_SCANCODE_KP_VERTICALBAR} = KeypadVerticalBar
sdlScanCode #{const SDL_SCANCODE_KP_DBLVERTICALBAR} = KeypadDoubleVerticalBar
sdlScanCode #{const SDL_SCANCODE_KP_COLON} = KeypadColon
sdlScanCode #{const SDL_SCANCODE_KP_HASH} = KeypadHash
sdlScanCode #{const SDL_SCANCODE_KP_SPACE} = KeypadSpace
sdlScanCode #{const SDL_SCANCODE_KP_AT} = KeypadAt
sdlScanCode #{const SDL_SCANCODE_KP_EXCLAM} = KeypadExclamation
sdlScanCode #{const SDL_SCANCODE_KP_MEMSTORE} = KeypadMemStore
sdlScanCode #{const SDL_SCANCODE_KP_MEMRECALL} = KeypadMemRecall
sdlScanCode #{const SDL_SCANCODE_KP_MEMCLEAR} = KeypadMemClear
sdlScanCode #{const SDL_SCANCODE_KP_MEMADD} = KeypadMemAdd
sdlScanCode #{const SDL_SCANCODE_KP_MEMSUBTRACT} = KeypadMemSubstract
sdlScanCode #{const SDL_SCANCODE_KP_MEMMULTIPLY} = KeypadMemMultiply
sdlScanCode #{const SDL_SCANCODE_KP_MEMDIVIDE} = KeypadMemDivide
sdlScanCode #{const SDL_SCANCODE_KP_PLUSMINUS} = KeypadPlusMinus
sdlScanCode #{const SDL_SCANCODE_KP_CLEAR} = KeypadClear
sdlScanCode #{const SDL_SCANCODE_KP_CLEARENTRY} = KeypadClearEntry
sdlScanCode #{const SDL_SCANCODE_KP_BINARY} = KeypadBinary
sdlScanCode #{const SDL_SCANCODE_KP_OCTAL} = KeypadOctal
sdlScanCode #{const SDL_SCANCODE_KP_DECIMAL} = KeypadDecimal
sdlScanCode #{const SDL_SCANCODE_KP_HEXADECIMAL} = KeypadHexadecimal
sdlScanCode #{const SDL_SCANCODE_LCTRL} = LeftControl
sdlScanCode #{const SDL_SCANCODE_LSHIFT} = LeftShift
sdlScanCode #{const SDL_SCANCODE_LALT} = LeftAlt
sdlScanCode #{const SDL_SCANCODE_LGUI} = LeftGUI
sdlScanCode #{const SDL_SCANCODE_RCTRL} = RightControl
sdlScanCode #{const SDL_SCANCODE_RSHIFT} = RightShift
sdlScanCode #{const SDL_SCANCODE_RALT} = RightAlt
sdlScanCode #{const SDL_SCANCODE_RGUI} = RightGUI
sdlScanCode #{const SDL_SCANCODE_MODE} = Mode
sdlScanCode #{const SDL_SCANCODE_AUDIONEXT} = AudioNext
sdlScanCode #{const SDL_SCANCODE_AUDIOPREV} = AudioPrevious
sdlScanCode #{const SDL_SCANCODE_AUDIOSTOP} = AudioStop
sdlScanCode #{const SDL_SCANCODE_AUDIOPLAY} = AudioPlay
sdlScanCode #{const SDL_SCANCODE_AUDIOMUTE} = AudioMute
sdlScanCode #{const SDL_SCANCODE_MEDIASELECT} = MediaSelect
sdlScanCode #{const SDL_SCANCODE_WWW} = WWW
sdlScanCode #{const SDL_SCANCODE_MAIL} = Mail
sdlScanCode #{const SDL_SCANCODE_CALCULATOR} = Calculator
sdlScanCode #{const SDL_SCANCODE_COMPUTER} = Computer
sdlScanCode #{const SDL_SCANCODE_AC_SEARCH} = ACSearch
sdlScanCode #{const SDL_SCANCODE_AC_HOME} = ACHome
sdlScanCode #{const SDL_SCANCODE_AC_BACK} = ACBack
sdlScanCode #{const SDL_SCANCODE_AC_FORWARD} = ACForward
sdlScanCode #{const SDL_SCANCODE_AC_STOP} = ACStop
sdlScanCode #{const SDL_SCANCODE_AC_REFRESH} = ACRefresh
sdlScanCode #{const SDL_SCANCODE_AC_BOOKMARKS} = ACBookmarks
sdlScanCode #{const SDL_SCANCODE_BRIGHTNESSDOWN} = BrightnessDown
sdlScanCode #{const SDL_SCANCODE_BRIGHTNESSUP} = BrightnessUp
sdlScanCode #{const SDL_SCANCODE_DISPLAYSWITCH} = DisplaySwitch
sdlScanCode #{const SDL_SCANCODE_KBDILLUMTOGGLE} = KBIllumToggle
sdlScanCode #{const SDL_SCANCODE_KBDILLUMDOWN} = KBIllumDown
sdlScanCode #{const SDL_SCANCODE_KBDILLUMUP} = KBIllumUp
sdlScanCode #{const SDL_SCANCODE_EJECT} = Eject
sdlScanCode #{const SDL_SCANCODE_SLEEP} = Sleep
sdlScanCode #{const SDL_SCANCODE_APP1} = App1
sdlScanCode #{const SDL_SCANCODE_APP2} = App2
