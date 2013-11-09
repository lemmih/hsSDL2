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
import Graphics.UI.SDL.Keycode (Keycode)

data Keysym = Keysym { keyScancode :: Scancode
                     , keyKeycode :: Keycode
                     , keyModifiers :: Word16
                     }
  deriving (Eq, Show)

instance Storable Keysym where
  sizeOf = const #{size SDL_Keysym}

  alignment = const 4

  poke ptr (Keysym _s _k m) = do
    -- TODO? Do we care about poking keysyms?
    -- #{poke SDL_Keysym, scancode} ptr s
    -- #{poke SDL_Keysym, sym} ptr k
    #{poke SDL_Keysym, mod} ptr m
    #{poke SDL_Keysym, unused} ptr (0 :: Word32)

  peek ptr = Keysym <$> (toEnum <$> #{peek SDL_Keysym, scancode} ptr)
                    <*> (toEnum <$> #{peek SDL_Keysym, sym} ptr)
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

instance Enum Scancode where
  toEnum #{const SDL_SCANCODE_A} = A
  toEnum #{const SDL_SCANCODE_B} = B
  toEnum #{const SDL_SCANCODE_C} = C
  toEnum #{const SDL_SCANCODE_D} = D
  toEnum #{const SDL_SCANCODE_E} = E
  toEnum #{const SDL_SCANCODE_F} = F
  toEnum #{const SDL_SCANCODE_G} = G
  toEnum #{const SDL_SCANCODE_H} = H
  toEnum #{const SDL_SCANCODE_I} = I
  toEnum #{const SDL_SCANCODE_J} = J
  toEnum #{const SDL_SCANCODE_K} = K
  toEnum #{const SDL_SCANCODE_L} = L
  toEnum #{const SDL_SCANCODE_M} = M
  toEnum #{const SDL_SCANCODE_N} = N
  toEnum #{const SDL_SCANCODE_O} = O
  toEnum #{const SDL_SCANCODE_P} = P
  toEnum #{const SDL_SCANCODE_Q} = Q
  toEnum #{const SDL_SCANCODE_R} = R
  toEnum #{const SDL_SCANCODE_S} = S
  toEnum #{const SDL_SCANCODE_T} = T
  toEnum #{const SDL_SCANCODE_U} = U
  toEnum #{const SDL_SCANCODE_V} = V
  toEnum #{const SDL_SCANCODE_W} = W
  toEnum #{const SDL_SCANCODE_X} = X
  toEnum #{const SDL_SCANCODE_Y} = Y
  toEnum #{const SDL_SCANCODE_Z} = Z
  toEnum #{const SDL_SCANCODE_1} = Number1
  toEnum #{const SDL_SCANCODE_2} = Number2
  toEnum #{const SDL_SCANCODE_3} = Number3
  toEnum #{const SDL_SCANCODE_4} = Number4
  toEnum #{const SDL_SCANCODE_5} = Number5
  toEnum #{const SDL_SCANCODE_6} = Number6
  toEnum #{const SDL_SCANCODE_7} = Number7
  toEnum #{const SDL_SCANCODE_8} = Number8
  toEnum #{const SDL_SCANCODE_9} = Number9
  toEnum #{const SDL_SCANCODE_0} = Number0
  toEnum #{const SDL_SCANCODE_RETURN} = Return
  toEnum #{const SDL_SCANCODE_ESCAPE} = Escape
  toEnum #{const SDL_SCANCODE_BACKSPACE} = Backspace
  toEnum #{const SDL_SCANCODE_TAB} = Tab
  toEnum #{const SDL_SCANCODE_SPACE} = Space
  toEnum #{const SDL_SCANCODE_MINUS} = Minus
  toEnum #{const SDL_SCANCODE_EQUALS} = Equals
  toEnum #{const SDL_SCANCODE_LEFTBRACKET} = LeftBracket
  toEnum #{const SDL_SCANCODE_RIGHTBRACKET} = RightBracket
  toEnum #{const SDL_SCANCODE_BACKSLASH} = Backslash
  toEnum #{const SDL_SCANCODE_NONUSHASH} = NonUSHash
  toEnum #{const SDL_SCANCODE_SEMICOLON} = Semicolon
  toEnum #{const SDL_SCANCODE_APOSTROPHE} = Apostrophe
  toEnum #{const SDL_SCANCODE_GRAVE} = Grave
  toEnum #{const SDL_SCANCODE_COMMA} = Comma
  toEnum #{const SDL_SCANCODE_PERIOD} = Period
  toEnum #{const SDL_SCANCODE_SLASH} = Slash
  toEnum #{const SDL_SCANCODE_CAPSLOCK} = Capslock
  toEnum #{const SDL_SCANCODE_F1} = F1
  toEnum #{const SDL_SCANCODE_F2} = F2
  toEnum #{const SDL_SCANCODE_F3} = F3
  toEnum #{const SDL_SCANCODE_F4} = F4
  toEnum #{const SDL_SCANCODE_F5} = F5
  toEnum #{const SDL_SCANCODE_F6} = F6
  toEnum #{const SDL_SCANCODE_F7} = F7
  toEnum #{const SDL_SCANCODE_F8} = F8
  toEnum #{const SDL_SCANCODE_F9} = F9
  toEnum #{const SDL_SCANCODE_F10} = F10
  toEnum #{const SDL_SCANCODE_F11} = F11
  toEnum #{const SDL_SCANCODE_F12} = F12
  toEnum #{const SDL_SCANCODE_PRINTSCREEN} = PrintScreen
  toEnum #{const SDL_SCANCODE_SCROLLLOCK} = ScrollLock
  toEnum #{const SDL_SCANCODE_PAUSE} = Pause
  toEnum #{const SDL_SCANCODE_INSERT} = Insert
  toEnum #{const SDL_SCANCODE_HOME} = Home
  toEnum #{const SDL_SCANCODE_PAGEUP} = PageUp
  toEnum #{const SDL_SCANCODE_DELETE} = Delete
  toEnum #{const SDL_SCANCODE_END} = End
  toEnum #{const SDL_SCANCODE_PAGEDOWN} = PageDown
  toEnum #{const SDL_SCANCODE_RIGHT} = Right
  toEnum #{const SDL_SCANCODE_LEFT} = Left
  toEnum #{const SDL_SCANCODE_DOWN} = Down
  toEnum #{const SDL_SCANCODE_UP} = Up
  toEnum #{const SDL_SCANCODE_NUMLOCKCLEAR} = NumLockClear
  toEnum #{const SDL_SCANCODE_KP_DIVIDE} = KeypadDivide
  toEnum #{const SDL_SCANCODE_KP_MULTIPLY} = KeypadMultiply
  toEnum #{const SDL_SCANCODE_KP_MINUS} = KeypadMinus
  toEnum #{const SDL_SCANCODE_KP_PLUS} = KeypadPlus
  toEnum #{const SDL_SCANCODE_KP_ENTER} = KeypadEnter
  toEnum #{const SDL_SCANCODE_KP_1} = Keypad1
  toEnum #{const SDL_SCANCODE_KP_2} = Keypad2
  toEnum #{const SDL_SCANCODE_KP_3} = Keypad3
  toEnum #{const SDL_SCANCODE_KP_4} = Keypad4
  toEnum #{const SDL_SCANCODE_KP_5} = Keypad5
  toEnum #{const SDL_SCANCODE_KP_6} = Keypad6
  toEnum #{const SDL_SCANCODE_KP_7} = Keypad7
  toEnum #{const SDL_SCANCODE_KP_8} = Keypad8
  toEnum #{const SDL_SCANCODE_KP_9} = Keypad9
  toEnum #{const SDL_SCANCODE_KP_0} = Keypad0
  toEnum #{const SDL_SCANCODE_KP_PERIOD} = KeypadPeriod
  toEnum #{const SDL_SCANCODE_NONUSBACKSLASH} = NonUSBackslash
  toEnum #{const SDL_SCANCODE_APPLICATION} = Application
  toEnum #{const SDL_SCANCODE_POWER} = Power
  toEnum #{const SDL_SCANCODE_KP_EQUALS} = KeypadEquals
  toEnum #{const SDL_SCANCODE_F13} = F13
  toEnum #{const SDL_SCANCODE_F14} = F14
  toEnum #{const SDL_SCANCODE_F15} = F15
  toEnum #{const SDL_SCANCODE_F16} = F16
  toEnum #{const SDL_SCANCODE_F17} = F17
  toEnum #{const SDL_SCANCODE_F18} = F18
  toEnum #{const SDL_SCANCODE_F19} = F19
  toEnum #{const SDL_SCANCODE_F20} = F20
  toEnum #{const SDL_SCANCODE_F21} = F21
  toEnum #{const SDL_SCANCODE_F22} = F22
  toEnum #{const SDL_SCANCODE_F23} = F23
  toEnum #{const SDL_SCANCODE_F24} = F24
  toEnum #{const SDL_SCANCODE_EXECUTE} = Execute
  toEnum #{const SDL_SCANCODE_HELP} = Help
  toEnum #{const SDL_SCANCODE_MENU} = Menu
  toEnum #{const SDL_SCANCODE_SELECT} = Select
  toEnum #{const SDL_SCANCODE_STOP} = Stop
  toEnum #{const SDL_SCANCODE_AGAIN} = Again
  toEnum #{const SDL_SCANCODE_UNDO} = Undo
  toEnum #{const SDL_SCANCODE_CUT} = Cut
  toEnum #{const SDL_SCANCODE_COPY} = Copy
  toEnum #{const SDL_SCANCODE_PASTE} = Paste
  toEnum #{const SDL_SCANCODE_FIND} = Find
  toEnum #{const SDL_SCANCODE_MUTE} = Mute
  toEnum #{const SDL_SCANCODE_VOLUMEUP} = VolumeUp
  toEnum #{const SDL_SCANCODE_VOLUMEDOWN} = VolumeDown
  toEnum #{const SDL_SCANCODE_KP_COMMA} = KeypadComma
  toEnum #{const SDL_SCANCODE_KP_EQUALSAS400} = KeyPadEqualsAs400
  toEnum #{const SDL_SCANCODE_INTERNATIONAL1} = International1
  toEnum #{const SDL_SCANCODE_INTERNATIONAL2} = International2
  toEnum #{const SDL_SCANCODE_INTERNATIONAL3} = International3
  toEnum #{const SDL_SCANCODE_INTERNATIONAL4} = International4
  toEnum #{const SDL_SCANCODE_INTERNATIONAL5} = International5
  toEnum #{const SDL_SCANCODE_INTERNATIONAL6} = International6
  toEnum #{const SDL_SCANCODE_INTERNATIONAL7} = International7
  toEnum #{const SDL_SCANCODE_INTERNATIONAL8} = International8
  toEnum #{const SDL_SCANCODE_INTERNATIONAL9} = International9
  toEnum #{const SDL_SCANCODE_LANG1} = Lang1
  toEnum #{const SDL_SCANCODE_LANG2} = Lang2
  toEnum #{const SDL_SCANCODE_LANG3} = Lang3
  toEnum #{const SDL_SCANCODE_LANG4} = Lang4
  toEnum #{const SDL_SCANCODE_LANG5} = Lang5
  toEnum #{const SDL_SCANCODE_LANG6} = Lang6
  toEnum #{const SDL_SCANCODE_LANG7} = Lang7
  toEnum #{const SDL_SCANCODE_LANG8} = Lang8
  toEnum #{const SDL_SCANCODE_LANG9} = Lang9
  toEnum #{const SDL_SCANCODE_ALTERASE} = AltErase
  toEnum #{const SDL_SCANCODE_SYSREQ} = SysReq
  toEnum #{const SDL_SCANCODE_CANCEL} = Cancel
  toEnum #{const SDL_SCANCODE_CLEAR} = Clear
  toEnum #{const SDL_SCANCODE_PRIOR} = Prior
  toEnum #{const SDL_SCANCODE_RETURN2} = Return2
  toEnum #{const SDL_SCANCODE_SEPARATOR} = Separator
  toEnum #{const SDL_SCANCODE_OUT} = Out
  toEnum #{const SDL_SCANCODE_OPER} = Oper
  toEnum #{const SDL_SCANCODE_CLEARAGAIN} = ClearAgain
  toEnum #{const SDL_SCANCODE_CRSEL} = CrSel
  toEnum #{const SDL_SCANCODE_EXSEL} = ExSel
  toEnum #{const SDL_SCANCODE_KP_00} = Keypad00
  toEnum #{const SDL_SCANCODE_KP_000} = Keypad000
  toEnum #{const SDL_SCANCODE_THOUSANDSSEPARATOR} = ThousandSeparator
  toEnum #{const SDL_SCANCODE_DECIMALSEPARATOR} = DecimalSeparator
  toEnum #{const SDL_SCANCODE_CURRENCYUNIT} = CurrencyUnit
  toEnum #{const SDL_SCANCODE_CURRENCYSUBUNIT} = CurrencySubunit
  toEnum #{const SDL_SCANCODE_KP_LEFTPAREN} = KeypadLeftParen
  toEnum #{const SDL_SCANCODE_KP_RIGHTPAREN} = KeypadRightParen
  toEnum #{const SDL_SCANCODE_KP_LEFTBRACE} = KeypadLeftBrace
  toEnum #{const SDL_SCANCODE_KP_RIGHTBRACE} = KeypadRightBrace
  toEnum #{const SDL_SCANCODE_KP_TAB} = KeypadTab
  toEnum #{const SDL_SCANCODE_KP_BACKSPACE} = KeypadBackspace
  toEnum #{const SDL_SCANCODE_KP_A} = KeypadA
  toEnum #{const SDL_SCANCODE_KP_B} = KeypadB
  toEnum #{const SDL_SCANCODE_KP_C} = KeypadC
  toEnum #{const SDL_SCANCODE_KP_D} = KeypadD
  toEnum #{const SDL_SCANCODE_KP_E} = KeypadE
  toEnum #{const SDL_SCANCODE_KP_F} = KeypadF
  toEnum #{const SDL_SCANCODE_KP_XOR} = KeypadXOR
  toEnum #{const SDL_SCANCODE_KP_POWER} = KeypadPower
  toEnum #{const SDL_SCANCODE_KP_PERCENT} = KeypadPercent
  toEnum #{const SDL_SCANCODE_KP_LESS} = KeypadLess
  toEnum #{const SDL_SCANCODE_KP_GREATER} = KeypadGreater
  toEnum #{const SDL_SCANCODE_KP_AMPERSAND} = KeypadAmpersand
  toEnum #{const SDL_SCANCODE_KP_DBLAMPERSAND} = KeypadDoubleAmpersand
  toEnum #{const SDL_SCANCODE_KP_VERTICALBAR} = KeypadVerticalBar
  toEnum #{const SDL_SCANCODE_KP_DBLVERTICALBAR} = KeypadDoubleVerticalBar
  toEnum #{const SDL_SCANCODE_KP_COLON} = KeypadColon
  toEnum #{const SDL_SCANCODE_KP_HASH} = KeypadHash
  toEnum #{const SDL_SCANCODE_KP_SPACE} = KeypadSpace
  toEnum #{const SDL_SCANCODE_KP_AT} = KeypadAt
  toEnum #{const SDL_SCANCODE_KP_EXCLAM} = KeypadExclamation
  toEnum #{const SDL_SCANCODE_KP_MEMSTORE} = KeypadMemStore
  toEnum #{const SDL_SCANCODE_KP_MEMRECALL} = KeypadMemRecall
  toEnum #{const SDL_SCANCODE_KP_MEMCLEAR} = KeypadMemClear
  toEnum #{const SDL_SCANCODE_KP_MEMADD} = KeypadMemAdd
  toEnum #{const SDL_SCANCODE_KP_MEMSUBTRACT} = KeypadMemSubstract
  toEnum #{const SDL_SCANCODE_KP_MEMMULTIPLY} = KeypadMemMultiply
  toEnum #{const SDL_SCANCODE_KP_MEMDIVIDE} = KeypadMemDivide
  toEnum #{const SDL_SCANCODE_KP_PLUSMINUS} = KeypadPlusMinus
  toEnum #{const SDL_SCANCODE_KP_CLEAR} = KeypadClear
  toEnum #{const SDL_SCANCODE_KP_CLEARENTRY} = KeypadClearEntry
  toEnum #{const SDL_SCANCODE_KP_BINARY} = KeypadBinary
  toEnum #{const SDL_SCANCODE_KP_OCTAL} = KeypadOctal
  toEnum #{const SDL_SCANCODE_KP_DECIMAL} = KeypadDecimal
  toEnum #{const SDL_SCANCODE_KP_HEXADECIMAL} = KeypadHexadecimal
  toEnum #{const SDL_SCANCODE_LCTRL} = LeftControl
  toEnum #{const SDL_SCANCODE_LSHIFT} = LeftShift
  toEnum #{const SDL_SCANCODE_LALT} = LeftAlt
  toEnum #{const SDL_SCANCODE_LGUI} = LeftGUI
  toEnum #{const SDL_SCANCODE_RCTRL} = RightControl
  toEnum #{const SDL_SCANCODE_RSHIFT} = RightShift
  toEnum #{const SDL_SCANCODE_RALT} = RightAlt
  toEnum #{const SDL_SCANCODE_RGUI} = RightGUI
  toEnum #{const SDL_SCANCODE_MODE} = Mode
  toEnum #{const SDL_SCANCODE_AUDIONEXT} = AudioNext
  toEnum #{const SDL_SCANCODE_AUDIOPREV} = AudioPrevious
  toEnum #{const SDL_SCANCODE_AUDIOSTOP} = AudioStop
  toEnum #{const SDL_SCANCODE_AUDIOPLAY} = AudioPlay
  toEnum #{const SDL_SCANCODE_AUDIOMUTE} = AudioMute
  toEnum #{const SDL_SCANCODE_MEDIASELECT} = MediaSelect
  toEnum #{const SDL_SCANCODE_WWW} = WWW
  toEnum #{const SDL_SCANCODE_MAIL} = Mail
  toEnum #{const SDL_SCANCODE_CALCULATOR} = Calculator
  toEnum #{const SDL_SCANCODE_COMPUTER} = Computer
  toEnum #{const SDL_SCANCODE_AC_SEARCH} = ACSearch
  toEnum #{const SDL_SCANCODE_AC_HOME} = ACHome
  toEnum #{const SDL_SCANCODE_AC_BACK} = ACBack
  toEnum #{const SDL_SCANCODE_AC_FORWARD} = ACForward
  toEnum #{const SDL_SCANCODE_AC_STOP} = ACStop
  toEnum #{const SDL_SCANCODE_AC_REFRESH} = ACRefresh
  toEnum #{const SDL_SCANCODE_AC_BOOKMARKS} = ACBookmarks
  toEnum #{const SDL_SCANCODE_BRIGHTNESSDOWN} = BrightnessDown
  toEnum #{const SDL_SCANCODE_BRIGHTNESSUP} = BrightnessUp
  toEnum #{const SDL_SCANCODE_DISPLAYSWITCH} = DisplaySwitch
  toEnum #{const SDL_SCANCODE_KBDILLUMTOGGLE} = KBIllumToggle
  toEnum #{const SDL_SCANCODE_KBDILLUMDOWN} = KBIllumDown
  toEnum #{const SDL_SCANCODE_KBDILLUMUP} = KBIllumUp
  toEnum #{const SDL_SCANCODE_EJECT} = Eject
  toEnum #{const SDL_SCANCODE_SLEEP} = Sleep
  toEnum #{const SDL_SCANCODE_APP1} = App1
  toEnum #{const SDL_SCANCODE_APP2} = App2
  toEnum _ = error "Scancode.toEnum: Invalid argument."

  fromEnum A = #{const SDL_SCANCODE_A}
  fromEnum B = #{const SDL_SCANCODE_B}
  fromEnum C = #{const SDL_SCANCODE_C}
  fromEnum D = #{const SDL_SCANCODE_D}
  fromEnum E = #{const SDL_SCANCODE_E}
  fromEnum F = #{const SDL_SCANCODE_F}
  fromEnum G = #{const SDL_SCANCODE_G}
  fromEnum H = #{const SDL_SCANCODE_H}
  fromEnum I = #{const SDL_SCANCODE_I}
  fromEnum J = #{const SDL_SCANCODE_J}
  fromEnum K = #{const SDL_SCANCODE_K}
  fromEnum L = #{const SDL_SCANCODE_L}
  fromEnum M = #{const SDL_SCANCODE_M}
  fromEnum N = #{const SDL_SCANCODE_N}
  fromEnum O = #{const SDL_SCANCODE_O}
  fromEnum P = #{const SDL_SCANCODE_P}
  fromEnum Q = #{const SDL_SCANCODE_Q}
  fromEnum R = #{const SDL_SCANCODE_R}
  fromEnum S = #{const SDL_SCANCODE_S}
  fromEnum T = #{const SDL_SCANCODE_T}
  fromEnum U = #{const SDL_SCANCODE_U}
  fromEnum V = #{const SDL_SCANCODE_V}
  fromEnum W = #{const SDL_SCANCODE_W}
  fromEnum X = #{const SDL_SCANCODE_X}
  fromEnum Y = #{const SDL_SCANCODE_Y}
  fromEnum Z = #{const SDL_SCANCODE_Z}
  fromEnum Number1 = #{const SDL_SCANCODE_1}
  fromEnum Number2 = #{const SDL_SCANCODE_2}
  fromEnum Number3 = #{const SDL_SCANCODE_3}
  fromEnum Number4 = #{const SDL_SCANCODE_4}
  fromEnum Number5 = #{const SDL_SCANCODE_5}
  fromEnum Number6 = #{const SDL_SCANCODE_6}
  fromEnum Number7 = #{const SDL_SCANCODE_7}
  fromEnum Number8 = #{const SDL_SCANCODE_8}
  fromEnum Number9 = #{const SDL_SCANCODE_9}
  fromEnum Number0 = #{const SDL_SCANCODE_0}
  fromEnum Return = #{const SDL_SCANCODE_RETURN}
  fromEnum Escape = #{const SDL_SCANCODE_ESCAPE}
  fromEnum Backspace = #{const SDL_SCANCODE_BACKSPACE}
  fromEnum Tab = #{const SDL_SCANCODE_TAB}
  fromEnum Space = #{const SDL_SCANCODE_SPACE}
  fromEnum Minus = #{const SDL_SCANCODE_MINUS}
  fromEnum Equals = #{const SDL_SCANCODE_EQUALS}
  fromEnum LeftBracket = #{const SDL_SCANCODE_LEFTBRACKET}
  fromEnum RightBracket = #{const SDL_SCANCODE_RIGHTBRACKET}
  fromEnum Backslash = #{const SDL_SCANCODE_BACKSLASH}
  fromEnum NonUSHash = #{const SDL_SCANCODE_NONUSHASH}
  fromEnum Semicolon = #{const SDL_SCANCODE_SEMICOLON}
  fromEnum Apostrophe = #{const SDL_SCANCODE_APOSTROPHE}
  fromEnum Grave = #{const SDL_SCANCODE_GRAVE}
  fromEnum Comma = #{const SDL_SCANCODE_COMMA}
  fromEnum Period = #{const SDL_SCANCODE_PERIOD}
  fromEnum Slash = #{const SDL_SCANCODE_SLASH}
  fromEnum Capslock = #{const SDL_SCANCODE_CAPSLOCK}
  fromEnum F1 = #{const SDL_SCANCODE_F1}
  fromEnum F2 = #{const SDL_SCANCODE_F2}
  fromEnum F3 = #{const SDL_SCANCODE_F3}
  fromEnum F4 = #{const SDL_SCANCODE_F4}
  fromEnum F5 = #{const SDL_SCANCODE_F5}
  fromEnum F6 = #{const SDL_SCANCODE_F6}
  fromEnum F7 = #{const SDL_SCANCODE_F7}
  fromEnum F8 = #{const SDL_SCANCODE_F8}
  fromEnum F9 = #{const SDL_SCANCODE_F9}
  fromEnum F10 = #{const SDL_SCANCODE_F10}
  fromEnum F11 = #{const SDL_SCANCODE_F11}
  fromEnum F12 = #{const SDL_SCANCODE_F12}
  fromEnum PrintScreen = #{const SDL_SCANCODE_PRINTSCREEN}
  fromEnum ScrollLock = #{const SDL_SCANCODE_SCROLLLOCK}
  fromEnum Pause = #{const SDL_SCANCODE_PAUSE}
  fromEnum Insert = #{const SDL_SCANCODE_INSERT}
  fromEnum Home = #{const SDL_SCANCODE_HOME}
  fromEnum PageUp = #{const SDL_SCANCODE_PAGEUP}
  fromEnum Delete = #{const SDL_SCANCODE_DELETE}
  fromEnum End = #{const SDL_SCANCODE_END}
  fromEnum PageDown = #{const SDL_SCANCODE_PAGEDOWN}
  fromEnum Right = #{const SDL_SCANCODE_RIGHT}
  fromEnum Left = #{const SDL_SCANCODE_LEFT}
  fromEnum Down = #{const SDL_SCANCODE_DOWN}
  fromEnum Up = #{const SDL_SCANCODE_UP}
  fromEnum NumLockClear = #{const SDL_SCANCODE_NUMLOCKCLEAR}
  fromEnum KeypadDivide = #{const SDL_SCANCODE_KP_DIVIDE}
  fromEnum KeypadMultiply = #{const SDL_SCANCODE_KP_MULTIPLY}
  fromEnum KeypadMinus = #{const SDL_SCANCODE_KP_MINUS}
  fromEnum KeypadPlus = #{const SDL_SCANCODE_KP_PLUS}
  fromEnum KeypadEnter = #{const SDL_SCANCODE_KP_ENTER}
  fromEnum Keypad1 = #{const SDL_SCANCODE_KP_1}
  fromEnum Keypad2 = #{const SDL_SCANCODE_KP_2}
  fromEnum Keypad3 = #{const SDL_SCANCODE_KP_3}
  fromEnum Keypad4 = #{const SDL_SCANCODE_KP_4}
  fromEnum Keypad5 = #{const SDL_SCANCODE_KP_5}
  fromEnum Keypad6 = #{const SDL_SCANCODE_KP_6}
  fromEnum Keypad7 = #{const SDL_SCANCODE_KP_7}
  fromEnum Keypad8 = #{const SDL_SCANCODE_KP_8}
  fromEnum Keypad9 = #{const SDL_SCANCODE_KP_9}
  fromEnum Keypad0 = #{const SDL_SCANCODE_KP_0}
  fromEnum KeypadPeriod = #{const SDL_SCANCODE_KP_PERIOD}
  fromEnum NonUSBackslash = #{const SDL_SCANCODE_NONUSBACKSLASH}
  fromEnum Application = #{const SDL_SCANCODE_APPLICATION}
  fromEnum Power = #{const SDL_SCANCODE_POWER}
  fromEnum KeypadEquals = #{const SDL_SCANCODE_KP_EQUALS}
  fromEnum F13 = #{const SDL_SCANCODE_F13}
  fromEnum F14 = #{const SDL_SCANCODE_F14}
  fromEnum F15 = #{const SDL_SCANCODE_F15}
  fromEnum F16 = #{const SDL_SCANCODE_F16}
  fromEnum F17 = #{const SDL_SCANCODE_F17}
  fromEnum F18 = #{const SDL_SCANCODE_F18}
  fromEnum F19 = #{const SDL_SCANCODE_F19}
  fromEnum F20 = #{const SDL_SCANCODE_F20}
  fromEnum F21 = #{const SDL_SCANCODE_F21}
  fromEnum F22 = #{const SDL_SCANCODE_F22}
  fromEnum F23 = #{const SDL_SCANCODE_F23}
  fromEnum F24 = #{const SDL_SCANCODE_F24}
  fromEnum Execute = #{const SDL_SCANCODE_EXECUTE}
  fromEnum Help = #{const SDL_SCANCODE_HELP}
  fromEnum Menu = #{const SDL_SCANCODE_MENU}
  fromEnum Select = #{const SDL_SCANCODE_SELECT}
  fromEnum Stop = #{const SDL_SCANCODE_STOP}
  fromEnum Again = #{const SDL_SCANCODE_AGAIN}
  fromEnum Undo = #{const SDL_SCANCODE_UNDO}
  fromEnum Cut = #{const SDL_SCANCODE_CUT}
  fromEnum Copy = #{const SDL_SCANCODE_COPY}
  fromEnum Paste = #{const SDL_SCANCODE_PASTE}
  fromEnum Find = #{const SDL_SCANCODE_FIND}
  fromEnum Mute = #{const SDL_SCANCODE_MUTE}
  fromEnum VolumeUp = #{const SDL_SCANCODE_VOLUMEUP}
  fromEnum VolumeDown = #{const SDL_SCANCODE_VOLUMEDOWN}
  fromEnum KeypadComma = #{const SDL_SCANCODE_KP_COMMA}
  fromEnum KeyPadEqualsAs400 = #{const SDL_SCANCODE_KP_EQUALSAS400}
  fromEnum International1 = #{const SDL_SCANCODE_INTERNATIONAL1}
  fromEnum International2 = #{const SDL_SCANCODE_INTERNATIONAL2}
  fromEnum International3 = #{const SDL_SCANCODE_INTERNATIONAL3}
  fromEnum International4 = #{const SDL_SCANCODE_INTERNATIONAL4}
  fromEnum International5 = #{const SDL_SCANCODE_INTERNATIONAL5}
  fromEnum International6 = #{const SDL_SCANCODE_INTERNATIONAL6}
  fromEnum International7 = #{const SDL_SCANCODE_INTERNATIONAL7}
  fromEnum International8 = #{const SDL_SCANCODE_INTERNATIONAL8}
  fromEnum International9 = #{const SDL_SCANCODE_INTERNATIONAL9}
  fromEnum Lang1 = #{const SDL_SCANCODE_LANG1}
  fromEnum Lang2 = #{const SDL_SCANCODE_LANG2}
  fromEnum Lang3 = #{const SDL_SCANCODE_LANG3}
  fromEnum Lang4 = #{const SDL_SCANCODE_LANG4}
  fromEnum Lang5 = #{const SDL_SCANCODE_LANG5}
  fromEnum Lang6 = #{const SDL_SCANCODE_LANG6}
  fromEnum Lang7 = #{const SDL_SCANCODE_LANG7}
  fromEnum Lang8 = #{const SDL_SCANCODE_LANG8}
  fromEnum Lang9 = #{const SDL_SCANCODE_LANG9}
  fromEnum AltErase = #{const SDL_SCANCODE_ALTERASE}
  fromEnum SysReq = #{const SDL_SCANCODE_SYSREQ}
  fromEnum Cancel = #{const SDL_SCANCODE_CANCEL}
  fromEnum Clear = #{const SDL_SCANCODE_CLEAR}
  fromEnum Prior = #{const SDL_SCANCODE_PRIOR}
  fromEnum Return2 = #{const SDL_SCANCODE_RETURN2}
  fromEnum Separator = #{const SDL_SCANCODE_SEPARATOR}
  fromEnum Out = #{const SDL_SCANCODE_OUT}
  fromEnum Oper = #{const SDL_SCANCODE_OPER}
  fromEnum ClearAgain = #{const SDL_SCANCODE_CLEARAGAIN}
  fromEnum CrSel = #{const SDL_SCANCODE_CRSEL}
  fromEnum ExSel = #{const SDL_SCANCODE_EXSEL}
  fromEnum Keypad00 = #{const SDL_SCANCODE_KP_00}
  fromEnum Keypad000 = #{const SDL_SCANCODE_KP_000}
  fromEnum ThousandSeparator = #{const SDL_SCANCODE_THOUSANDSSEPARATOR}
  fromEnum DecimalSeparator = #{const SDL_SCANCODE_DECIMALSEPARATOR}
  fromEnum CurrencyUnit = #{const SDL_SCANCODE_CURRENCYUNIT}
  fromEnum CurrencySubunit = #{const SDL_SCANCODE_CURRENCYSUBUNIT}
  fromEnum KeypadLeftParen = #{const SDL_SCANCODE_KP_LEFTPAREN}
  fromEnum KeypadRightParen = #{const SDL_SCANCODE_KP_RIGHTPAREN}
  fromEnum KeypadLeftBrace = #{const SDL_SCANCODE_KP_LEFTBRACE}
  fromEnum KeypadRightBrace = #{const SDL_SCANCODE_KP_RIGHTBRACE}
  fromEnum KeypadTab = #{const SDL_SCANCODE_KP_TAB}
  fromEnum KeypadBackspace = #{const SDL_SCANCODE_KP_BACKSPACE}
  fromEnum KeypadA = #{const SDL_SCANCODE_KP_A}
  fromEnum KeypadB = #{const SDL_SCANCODE_KP_B}
  fromEnum KeypadC = #{const SDL_SCANCODE_KP_C}
  fromEnum KeypadD = #{const SDL_SCANCODE_KP_D}
  fromEnum KeypadE = #{const SDL_SCANCODE_KP_E}
  fromEnum KeypadF = #{const SDL_SCANCODE_KP_F}
  fromEnum KeypadXOR = #{const SDL_SCANCODE_KP_XOR}
  fromEnum KeypadPower = #{const SDL_SCANCODE_KP_POWER}
  fromEnum KeypadPercent = #{const SDL_SCANCODE_KP_PERCENT}
  fromEnum KeypadLess = #{const SDL_SCANCODE_KP_LESS}
  fromEnum KeypadGreater = #{const SDL_SCANCODE_KP_GREATER}
  fromEnum KeypadAmpersand = #{const SDL_SCANCODE_KP_AMPERSAND}
  fromEnum KeypadDoubleAmpersand = #{const SDL_SCANCODE_KP_DBLAMPERSAND}
  fromEnum KeypadVerticalBar = #{const SDL_SCANCODE_KP_VERTICALBAR}
  fromEnum KeypadDoubleVerticalBar = #{const SDL_SCANCODE_KP_DBLVERTICALBAR}
  fromEnum KeypadColon = #{const SDL_SCANCODE_KP_COLON}
  fromEnum KeypadHash = #{const SDL_SCANCODE_KP_HASH}
  fromEnum KeypadSpace = #{const SDL_SCANCODE_KP_SPACE}
  fromEnum KeypadAt = #{const SDL_SCANCODE_KP_AT}
  fromEnum KeypadExclamation = #{const SDL_SCANCODE_KP_EXCLAM}
  fromEnum KeypadMemStore = #{const SDL_SCANCODE_KP_MEMSTORE}
  fromEnum KeypadMemRecall = #{const SDL_SCANCODE_KP_MEMRECALL}
  fromEnum KeypadMemClear = #{const SDL_SCANCODE_KP_MEMCLEAR}
  fromEnum KeypadMemAdd = #{const SDL_SCANCODE_KP_MEMADD}
  fromEnum KeypadMemSubstract = #{const SDL_SCANCODE_KP_MEMSUBTRACT}
  fromEnum KeypadMemMultiply = #{const SDL_SCANCODE_KP_MEMMULTIPLY}
  fromEnum KeypadMemDivide = #{const SDL_SCANCODE_KP_MEMDIVIDE}
  fromEnum KeypadPlusMinus = #{const SDL_SCANCODE_KP_PLUSMINUS}
  fromEnum KeypadClear = #{const SDL_SCANCODE_KP_CLEAR}
  fromEnum KeypadClearEntry = #{const SDL_SCANCODE_KP_CLEARENTRY}
  fromEnum KeypadBinary = #{const SDL_SCANCODE_KP_BINARY}
  fromEnum KeypadOctal = #{const SDL_SCANCODE_KP_OCTAL}
  fromEnum KeypadDecimal = #{const SDL_SCANCODE_KP_DECIMAL}
  fromEnum KeypadHexadecimal = #{const SDL_SCANCODE_KP_HEXADECIMAL}
  fromEnum LeftControl = #{const SDL_SCANCODE_LCTRL}
  fromEnum LeftShift = #{const SDL_SCANCODE_LSHIFT}
  fromEnum LeftAlt = #{const SDL_SCANCODE_LALT}
  fromEnum LeftGUI = #{const SDL_SCANCODE_LGUI}
  fromEnum RightControl = #{const SDL_SCANCODE_RCTRL}
  fromEnum RightShift = #{const SDL_SCANCODE_RSHIFT}
  fromEnum RightAlt = #{const SDL_SCANCODE_RALT}
  fromEnum RightGUI = #{const SDL_SCANCODE_RGUI}
  fromEnum Mode = #{const SDL_SCANCODE_MODE}
  fromEnum AudioNext = #{const SDL_SCANCODE_AUDIONEXT}
  fromEnum AudioPrevious = #{const SDL_SCANCODE_AUDIOPREV}
  fromEnum AudioStop = #{const SDL_SCANCODE_AUDIOSTOP}
  fromEnum AudioPlay = #{const SDL_SCANCODE_AUDIOPLAY}
  fromEnum AudioMute = #{const SDL_SCANCODE_AUDIOMUTE}
  fromEnum MediaSelect = #{const SDL_SCANCODE_MEDIASELECT}
  fromEnum WWW = #{const SDL_SCANCODE_WWW}
  fromEnum Mail = #{const SDL_SCANCODE_MAIL}
  fromEnum Calculator = #{const SDL_SCANCODE_CALCULATOR}
  fromEnum Computer = #{const SDL_SCANCODE_COMPUTER}
  fromEnum ACSearch = #{const SDL_SCANCODE_AC_SEARCH}
  fromEnum ACHome = #{const SDL_SCANCODE_AC_HOME}
  fromEnum ACBack = #{const SDL_SCANCODE_AC_BACK}
  fromEnum ACForward = #{const SDL_SCANCODE_AC_FORWARD}
  fromEnum ACStop = #{const SDL_SCANCODE_AC_STOP}
  fromEnum ACRefresh = #{const SDL_SCANCODE_AC_REFRESH}
  fromEnum ACBookmarks = #{const SDL_SCANCODE_AC_BOOKMARKS}
  fromEnum BrightnessDown = #{const SDL_SCANCODE_BRIGHTNESSDOWN}
  fromEnum BrightnessUp = #{const SDL_SCANCODE_BRIGHTNESSUP}
  fromEnum DisplaySwitch = #{const SDL_SCANCODE_DISPLAYSWITCH}
  fromEnum KBIllumToggle = #{const SDL_SCANCODE_KBDILLUMTOGGLE}
  fromEnum KBIllumDown = #{const SDL_SCANCODE_KBDILLUMDOWN}
  fromEnum KBIllumUp = #{const SDL_SCANCODE_KBDILLUMUP}
  fromEnum Eject = #{const SDL_SCANCODE_EJECT}
  fromEnum Sleep = #{const SDL_SCANCODE_SLEEP}
  fromEnum App1 = #{const SDL_SCANCODE_APP1}
  fromEnum App2 = #{const SDL_SCANCODE_APP2}
