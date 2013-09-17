#include "SDL.h"
module Graphics.UI.SDL.Keycode where

import Prelude hiding (Either(Left,Right))
import Foreign

data Keycode
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
  | Semicolon
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
  | Ampersand
  | Asterisk
  | At
  | Caret
  | Colon
  | Dollar
  | Exclaim
  | Greater
  | Hash
  | LeftParen
  | Less
  | Percent
  | Plus
  | Question
  | DoubleQuote
  | RightParen
  | Underscore
  deriving (Eq, Show)


sdlKeycode :: #{type SDL_Keycode} -> Keycode
sdlKeycode #{const SDLK_a} = A
sdlKeycode #{const SDLK_b} = B
sdlKeycode #{const SDLK_c} = C
sdlKeycode #{const SDLK_d} = D
sdlKeycode #{const SDLK_e} = E
sdlKeycode #{const SDLK_f} = F
sdlKeycode #{const SDLK_g} = G
sdlKeycode #{const SDLK_h} = H
sdlKeycode #{const SDLK_i} = I
sdlKeycode #{const SDLK_j} = J
sdlKeycode #{const SDLK_k} = K
sdlKeycode #{const SDLK_l} = L
sdlKeycode #{const SDLK_m} = M
sdlKeycode #{const SDLK_n} = N
sdlKeycode #{const SDLK_o} = O
sdlKeycode #{const SDLK_p} = P
sdlKeycode #{const SDLK_q} = Q
sdlKeycode #{const SDLK_r} = R
sdlKeycode #{const SDLK_s} = S
sdlKeycode #{const SDLK_t} = T
sdlKeycode #{const SDLK_u} = U
sdlKeycode #{const SDLK_v} = V
sdlKeycode #{const SDLK_w} = W
sdlKeycode #{const SDLK_x} = X
sdlKeycode #{const SDLK_y} = Y
sdlKeycode #{const SDLK_z} = Z
sdlKeycode #{const SDLK_1} = Number1
sdlKeycode #{const SDLK_2} = Number2
sdlKeycode #{const SDLK_3} = Number3
sdlKeycode #{const SDLK_4} = Number4
sdlKeycode #{const SDLK_5} = Number5
sdlKeycode #{const SDLK_6} = Number6
sdlKeycode #{const SDLK_7} = Number7
sdlKeycode #{const SDLK_8} = Number8
sdlKeycode #{const SDLK_9} = Number9
sdlKeycode #{const SDLK_0} = Number0
sdlKeycode #{const SDLK_RETURN} = Return
sdlKeycode #{const SDLK_ESCAPE} = Escape
sdlKeycode #{const SDLK_BACKSPACE} = Backspace
sdlKeycode #{const SDLK_TAB} = Tab
sdlKeycode #{const SDLK_SPACE} = Space
sdlKeycode #{const SDLK_MINUS} = Minus
sdlKeycode #{const SDLK_EQUALS} = Equals
sdlKeycode #{const SDLK_LEFTBRACKET} = LeftBracket
sdlKeycode #{const SDLK_RIGHTBRACKET} = RightBracket
sdlKeycode #{const SDLK_BACKSLASH} = Backslash
sdlKeycode #{const SDLK_SEMICOLON} = Semicolon
sdlKeycode #{const SDLK_COMMA} = Comma
sdlKeycode #{const SDLK_PERIOD} = Period
sdlKeycode #{const SDLK_SLASH} = Slash
sdlKeycode #{const SDLK_CAPSLOCK} = Capslock
sdlKeycode #{const SDLK_F1} = F1
sdlKeycode #{const SDLK_F2} = F2
sdlKeycode #{const SDLK_F3} = F3
sdlKeycode #{const SDLK_F4} = F4
sdlKeycode #{const SDLK_F5} = F5
sdlKeycode #{const SDLK_F6} = F6
sdlKeycode #{const SDLK_F7} = F7
sdlKeycode #{const SDLK_F8} = F8
sdlKeycode #{const SDLK_F9} = F9
sdlKeycode #{const SDLK_F10} = F10
sdlKeycode #{const SDLK_F11} = F11
sdlKeycode #{const SDLK_F12} = F12
sdlKeycode #{const SDLK_PRINTSCREEN} = PrintScreen
sdlKeycode #{const SDLK_SCROLLLOCK} = ScrollLock
sdlKeycode #{const SDLK_PAUSE} = Pause
sdlKeycode #{const SDLK_INSERT} = Insert
sdlKeycode #{const SDLK_HOME} = Home
sdlKeycode #{const SDLK_PAGEUP} = PageUp
sdlKeycode #{const SDLK_DELETE} = Delete
sdlKeycode #{const SDLK_END} = End
sdlKeycode #{const SDLK_PAGEDOWN} = PageDown
sdlKeycode #{const SDLK_RIGHT} = Right
sdlKeycode #{const SDLK_LEFT} = Left
sdlKeycode #{const SDLK_DOWN} = Down
sdlKeycode #{const SDLK_UP} = Up
sdlKeycode #{const SDLK_NUMLOCKCLEAR} = NumLockClear
sdlKeycode #{const SDLK_KP_DIVIDE} = KeypadDivide
sdlKeycode #{const SDLK_KP_MULTIPLY} = KeypadMultiply
sdlKeycode #{const SDLK_KP_MINUS} = KeypadMinus
sdlKeycode #{const SDLK_KP_PLUS} = KeypadPlus
sdlKeycode #{const SDLK_KP_ENTER} = KeypadEnter
sdlKeycode #{const SDLK_KP_1} = Keypad1
sdlKeycode #{const SDLK_KP_2} = Keypad2
sdlKeycode #{const SDLK_KP_3} = Keypad3
sdlKeycode #{const SDLK_KP_4} = Keypad4
sdlKeycode #{const SDLK_KP_5} = Keypad5
sdlKeycode #{const SDLK_KP_6} = Keypad6
sdlKeycode #{const SDLK_KP_7} = Keypad7
sdlKeycode #{const SDLK_KP_8} = Keypad8
sdlKeycode #{const SDLK_KP_9} = Keypad9
sdlKeycode #{const SDLK_KP_0} = Keypad0
sdlKeycode #{const SDLK_KP_PERIOD} = KeypadPeriod
sdlKeycode #{const SDLK_APPLICATION} = Application
sdlKeycode #{const SDLK_POWER} = Power
sdlKeycode #{const SDLK_KP_EQUALS} = KeypadEquals
sdlKeycode #{const SDLK_F13} = F13
sdlKeycode #{const SDLK_F14} = F14
sdlKeycode #{const SDLK_F15} = F15
sdlKeycode #{const SDLK_F16} = F16
sdlKeycode #{const SDLK_F17} = F17
sdlKeycode #{const SDLK_F18} = F18
sdlKeycode #{const SDLK_F19} = F19
sdlKeycode #{const SDLK_F20} = F20
sdlKeycode #{const SDLK_F21} = F21
sdlKeycode #{const SDLK_F22} = F22
sdlKeycode #{const SDLK_F23} = F23
sdlKeycode #{const SDLK_F24} = F24
sdlKeycode #{const SDLK_EXECUTE} = Execute
sdlKeycode #{const SDLK_HELP} = Help
sdlKeycode #{const SDLK_MENU} = Menu
sdlKeycode #{const SDLK_SELECT} = Select
sdlKeycode #{const SDLK_STOP} = Stop
sdlKeycode #{const SDLK_AGAIN} = Again
sdlKeycode #{const SDLK_UNDO} = Undo
sdlKeycode #{const SDLK_CUT} = Cut
sdlKeycode #{const SDLK_COPY} = Copy
sdlKeycode #{const SDLK_PASTE} = Paste
sdlKeycode #{const SDLK_FIND} = Find
sdlKeycode #{const SDLK_MUTE} = Mute
sdlKeycode #{const SDLK_VOLUMEUP} = VolumeUp
sdlKeycode #{const SDLK_VOLUMEDOWN} = VolumeDown
sdlKeycode #{const SDLK_KP_COMMA} = KeypadComma
sdlKeycode #{const SDLK_KP_EQUALSAS400} = KeyPadEqualsAs400
sdlKeycode #{const SDLK_ALTERASE} = AltErase
sdlKeycode #{const SDLK_SYSREQ} = SysReq
sdlKeycode #{const SDLK_CANCEL} = Cancel
sdlKeycode #{const SDLK_CLEAR} = Clear
sdlKeycode #{const SDLK_PRIOR} = Prior
sdlKeycode #{const SDLK_RETURN2} = Return2
sdlKeycode #{const SDLK_SEPARATOR} = Separator
sdlKeycode #{const SDLK_OUT} = Out
sdlKeycode #{const SDLK_OPER} = Oper
sdlKeycode #{const SDLK_CLEARAGAIN} = ClearAgain
sdlKeycode #{const SDLK_CRSEL} = CrSel
sdlKeycode #{const SDLK_EXSEL} = ExSel
sdlKeycode #{const SDLK_KP_00} = Keypad00
sdlKeycode #{const SDLK_KP_000} = Keypad000
sdlKeycode #{const SDLK_THOUSANDSSEPARATOR} = ThousandSeparator
sdlKeycode #{const SDLK_DECIMALSEPARATOR} = DecimalSeparator
sdlKeycode #{const SDLK_CURRENCYUNIT} = CurrencyUnit
sdlKeycode #{const SDLK_CURRENCYSUBUNIT} = CurrencySubunit
sdlKeycode #{const SDLK_KP_LEFTPAREN} = KeypadLeftParen
sdlKeycode #{const SDLK_KP_RIGHTPAREN} = KeypadRightParen
sdlKeycode #{const SDLK_KP_LEFTBRACE} = KeypadLeftBrace
sdlKeycode #{const SDLK_KP_RIGHTBRACE} = KeypadRightBrace
sdlKeycode #{const SDLK_KP_TAB} = KeypadTab
sdlKeycode #{const SDLK_KP_BACKSPACE} = KeypadBackspace
sdlKeycode #{const SDLK_KP_A} = KeypadA
sdlKeycode #{const SDLK_KP_B} = KeypadB
sdlKeycode #{const SDLK_KP_C} = KeypadC
sdlKeycode #{const SDLK_KP_D} = KeypadD
sdlKeycode #{const SDLK_KP_E} = KeypadE
sdlKeycode #{const SDLK_KP_F} = KeypadF
sdlKeycode #{const SDLK_KP_XOR} = KeypadXOR
sdlKeycode #{const SDLK_KP_POWER} = KeypadPower
sdlKeycode #{const SDLK_KP_PERCENT} = KeypadPercent
sdlKeycode #{const SDLK_KP_LESS} = KeypadLess
sdlKeycode #{const SDLK_KP_GREATER} = KeypadGreater
sdlKeycode #{const SDLK_KP_AMPERSAND} = KeypadAmpersand
sdlKeycode #{const SDLK_KP_DBLAMPERSAND} = KeypadDoubleAmpersand
sdlKeycode #{const SDLK_KP_VERTICALBAR} = KeypadVerticalBar
sdlKeycode #{const SDLK_KP_DBLVERTICALBAR} = KeypadDoubleVerticalBar
sdlKeycode #{const SDLK_KP_COLON} = KeypadColon
sdlKeycode #{const SDLK_KP_HASH} = KeypadHash
sdlKeycode #{const SDLK_KP_SPACE} = KeypadSpace
sdlKeycode #{const SDLK_KP_AT} = KeypadAt
sdlKeycode #{const SDLK_KP_EXCLAM} = KeypadExclamation
sdlKeycode #{const SDLK_KP_MEMSTORE} = KeypadMemStore
sdlKeycode #{const SDLK_KP_MEMRECALL} = KeypadMemRecall
sdlKeycode #{const SDLK_KP_MEMCLEAR} = KeypadMemClear
sdlKeycode #{const SDLK_KP_MEMADD} = KeypadMemAdd
sdlKeycode #{const SDLK_KP_MEMSUBTRACT} = KeypadMemSubstract
sdlKeycode #{const SDLK_KP_MEMMULTIPLY} = KeypadMemMultiply
sdlKeycode #{const SDLK_KP_MEMDIVIDE} = KeypadMemDivide
sdlKeycode #{const SDLK_KP_PLUSMINUS} = KeypadPlusMinus
sdlKeycode #{const SDLK_KP_CLEAR} = KeypadClear
sdlKeycode #{const SDLK_KP_CLEARENTRY} = KeypadClearEntry
sdlKeycode #{const SDLK_KP_BINARY} = KeypadBinary
sdlKeycode #{const SDLK_KP_OCTAL} = KeypadOctal
sdlKeycode #{const SDLK_KP_DECIMAL} = KeypadDecimal
sdlKeycode #{const SDLK_KP_HEXADECIMAL} = KeypadHexadecimal
sdlKeycode #{const SDLK_LCTRL} = LeftControl
sdlKeycode #{const SDLK_LSHIFT} = LeftShift
sdlKeycode #{const SDLK_LALT} = LeftAlt
sdlKeycode #{const SDLK_LGUI} = LeftGUI
sdlKeycode #{const SDLK_RCTRL} = RightControl
sdlKeycode #{const SDLK_RSHIFT} = RightShift
sdlKeycode #{const SDLK_RALT} = RightAlt
sdlKeycode #{const SDLK_RGUI} = RightGUI
sdlKeycode #{const SDLK_MODE} = Mode
sdlKeycode #{const SDLK_AUDIONEXT} = AudioNext
sdlKeycode #{const SDLK_AUDIOPREV} = AudioPrevious
sdlKeycode #{const SDLK_AUDIOSTOP} = AudioStop
sdlKeycode #{const SDLK_AUDIOPLAY} = AudioPlay
sdlKeycode #{const SDLK_AUDIOMUTE} = AudioMute
sdlKeycode #{const SDLK_MEDIASELECT} = MediaSelect
sdlKeycode #{const SDLK_WWW} = WWW
sdlKeycode #{const SDLK_MAIL} = Mail
sdlKeycode #{const SDLK_CALCULATOR} = Calculator
sdlKeycode #{const SDLK_COMPUTER} = Computer
sdlKeycode #{const SDLK_AC_SEARCH} = ACSearch
sdlKeycode #{const SDLK_AC_HOME} = ACHome
sdlKeycode #{const SDLK_AC_BACK} = ACBack
sdlKeycode #{const SDLK_AC_FORWARD} = ACForward
sdlKeycode #{const SDLK_AC_STOP} = ACStop
sdlKeycode #{const SDLK_AC_REFRESH} = ACRefresh
sdlKeycode #{const SDLK_AC_BOOKMARKS} = ACBookmarks
sdlKeycode #{const SDLK_BRIGHTNESSDOWN} = BrightnessDown
sdlKeycode #{const SDLK_BRIGHTNESSUP} = BrightnessUp
sdlKeycode #{const SDLK_DISPLAYSWITCH} = DisplaySwitch
sdlKeycode #{const SDLK_KBDILLUMTOGGLE} = KBIllumToggle
sdlKeycode #{const SDLK_KBDILLUMDOWN} = KBIllumDown
sdlKeycode #{const SDLK_KBDILLUMUP} = KBIllumUp
sdlKeycode #{const SDLK_EJECT} = Eject
sdlKeycode #{const SDLK_SLEEP} = Sleep
sdlKeycode #{const SDLK_AMPERSAND} = Ampersand
sdlKeycode #{const SDLK_ASTERISK} = Asterisk
sdlKeycode #{const SDLK_AT} = At
sdlKeycode #{const SDLK_CARET} = Caret
sdlKeycode #{const SDLK_COLON} = Colon
sdlKeycode #{const SDLK_DOLLAR} = Dollar
sdlKeycode #{const SDLK_EXCLAIM} = Exclaim
sdlKeycode #{const SDLK_GREATER} = Greater 
sdlKeycode #{const SDLK_HASH} = Hash
sdlKeycode #{const SDLK_LEFTPAREN} = LeftParen
sdlKeycode #{const SDLK_LESS} = Less
sdlKeycode #{const SDLK_PERCENT} = Percent
sdlKeycode #{const SDLK_PLUS} = Plus
sdlKeycode #{const SDLK_QUESTION} = Question
sdlKeycode #{const SDLK_QUOTEDBL} = DoubleQuote
sdlKeycode #{const SDLK_RIGHTPAREN} = RightParen
sdlKeycode #{const SDLK_UNDERSCORE} = Underscore
