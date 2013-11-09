#include "SDL.h"
module Graphics.UI.SDL.Keycode where

import Prelude hiding (Either(Left,Right))

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

instance Enum Keycode where
  toEnum #{const SDLK_a} = A
  toEnum #{const SDLK_b} = B
  toEnum #{const SDLK_c} = C
  toEnum #{const SDLK_d} = D
  toEnum #{const SDLK_e} = E
  toEnum #{const SDLK_f} = F
  toEnum #{const SDLK_g} = G
  toEnum #{const SDLK_h} = H
  toEnum #{const SDLK_i} = I
  toEnum #{const SDLK_j} = J
  toEnum #{const SDLK_k} = K
  toEnum #{const SDLK_l} = L
  toEnum #{const SDLK_m} = M
  toEnum #{const SDLK_n} = N
  toEnum #{const SDLK_o} = O
  toEnum #{const SDLK_p} = P
  toEnum #{const SDLK_q} = Q
  toEnum #{const SDLK_r} = R
  toEnum #{const SDLK_s} = S
  toEnum #{const SDLK_t} = T
  toEnum #{const SDLK_u} = U
  toEnum #{const SDLK_v} = V
  toEnum #{const SDLK_w} = W
  toEnum #{const SDLK_x} = X
  toEnum #{const SDLK_y} = Y
  toEnum #{const SDLK_z} = Z
  toEnum #{const SDLK_1} = Number1
  toEnum #{const SDLK_2} = Number2
  toEnum #{const SDLK_3} = Number3
  toEnum #{const SDLK_4} = Number4
  toEnum #{const SDLK_5} = Number5
  toEnum #{const SDLK_6} = Number6
  toEnum #{const SDLK_7} = Number7
  toEnum #{const SDLK_8} = Number8
  toEnum #{const SDLK_9} = Number9
  toEnum #{const SDLK_0} = Number0
  toEnum #{const SDLK_RETURN} = Return
  toEnum #{const SDLK_ESCAPE} = Escape
  toEnum #{const SDLK_BACKSPACE} = Backspace
  toEnum #{const SDLK_TAB} = Tab
  toEnum #{const SDLK_SPACE} = Space
  toEnum #{const SDLK_MINUS} = Minus
  toEnum #{const SDLK_EQUALS} = Equals
  toEnum #{const SDLK_LEFTBRACKET} = LeftBracket
  toEnum #{const SDLK_RIGHTBRACKET} = RightBracket
  toEnum #{const SDLK_BACKSLASH} = Backslash
  toEnum #{const SDLK_SEMICOLON} = Semicolon
  toEnum #{const SDLK_COMMA} = Comma
  toEnum #{const SDLK_PERIOD} = Period
  toEnum #{const SDLK_SLASH} = Slash
  toEnum #{const SDLK_CAPSLOCK} = Capslock
  toEnum #{const SDLK_F1} = F1
  toEnum #{const SDLK_F2} = F2
  toEnum #{const SDLK_F3} = F3
  toEnum #{const SDLK_F4} = F4
  toEnum #{const SDLK_F5} = F5
  toEnum #{const SDLK_F6} = F6
  toEnum #{const SDLK_F7} = F7
  toEnum #{const SDLK_F8} = F8
  toEnum #{const SDLK_F9} = F9
  toEnum #{const SDLK_F10} = F10
  toEnum #{const SDLK_F11} = F11
  toEnum #{const SDLK_F12} = F12
  toEnum #{const SDLK_PRINTSCREEN} = PrintScreen
  toEnum #{const SDLK_SCROLLLOCK} = ScrollLock
  toEnum #{const SDLK_PAUSE} = Pause
  toEnum #{const SDLK_INSERT} = Insert
  toEnum #{const SDLK_HOME} = Home
  toEnum #{const SDLK_PAGEUP} = PageUp
  toEnum #{const SDLK_DELETE} = Delete
  toEnum #{const SDLK_END} = End
  toEnum #{const SDLK_PAGEDOWN} = PageDown
  toEnum #{const SDLK_RIGHT} = Right
  toEnum #{const SDLK_LEFT} = Left
  toEnum #{const SDLK_DOWN} = Down
  toEnum #{const SDLK_UP} = Up
  toEnum #{const SDLK_NUMLOCKCLEAR} = NumLockClear
  toEnum #{const SDLK_KP_DIVIDE} = KeypadDivide
  toEnum #{const SDLK_KP_MULTIPLY} = KeypadMultiply
  toEnum #{const SDLK_KP_MINUS} = KeypadMinus
  toEnum #{const SDLK_KP_PLUS} = KeypadPlus
  toEnum #{const SDLK_KP_ENTER} = KeypadEnter
  toEnum #{const SDLK_KP_1} = Keypad1
  toEnum #{const SDLK_KP_2} = Keypad2
  toEnum #{const SDLK_KP_3} = Keypad3
  toEnum #{const SDLK_KP_4} = Keypad4
  toEnum #{const SDLK_KP_5} = Keypad5
  toEnum #{const SDLK_KP_6} = Keypad6
  toEnum #{const SDLK_KP_7} = Keypad7
  toEnum #{const SDLK_KP_8} = Keypad8
  toEnum #{const SDLK_KP_9} = Keypad9
  toEnum #{const SDLK_KP_0} = Keypad0
  toEnum #{const SDLK_KP_PERIOD} = KeypadPeriod
  toEnum #{const SDLK_APPLICATION} = Application
  toEnum #{const SDLK_POWER} = Power
  toEnum #{const SDLK_KP_EQUALS} = KeypadEquals
  toEnum #{const SDLK_F13} = F13
  toEnum #{const SDLK_F14} = F14
  toEnum #{const SDLK_F15} = F15
  toEnum #{const SDLK_F16} = F16
  toEnum #{const SDLK_F17} = F17
  toEnum #{const SDLK_F18} = F18
  toEnum #{const SDLK_F19} = F19
  toEnum #{const SDLK_F20} = F20
  toEnum #{const SDLK_F21} = F21
  toEnum #{const SDLK_F22} = F22
  toEnum #{const SDLK_F23} = F23
  toEnum #{const SDLK_F24} = F24
  toEnum #{const SDLK_EXECUTE} = Execute
  toEnum #{const SDLK_HELP} = Help
  toEnum #{const SDLK_MENU} = Menu
  toEnum #{const SDLK_SELECT} = Select
  toEnum #{const SDLK_STOP} = Stop
  toEnum #{const SDLK_AGAIN} = Again
  toEnum #{const SDLK_UNDO} = Undo
  toEnum #{const SDLK_CUT} = Cut
  toEnum #{const SDLK_COPY} = Copy
  toEnum #{const SDLK_PASTE} = Paste
  toEnum #{const SDLK_FIND} = Find
  toEnum #{const SDLK_MUTE} = Mute
  toEnum #{const SDLK_VOLUMEUP} = VolumeUp
  toEnum #{const SDLK_VOLUMEDOWN} = VolumeDown
  toEnum #{const SDLK_KP_COMMA} = KeypadComma
  toEnum #{const SDLK_KP_EQUALSAS400} = KeyPadEqualsAs400
  toEnum #{const SDLK_ALTERASE} = AltErase
  toEnum #{const SDLK_SYSREQ} = SysReq
  toEnum #{const SDLK_CANCEL} = Cancel
  toEnum #{const SDLK_CLEAR} = Clear
  toEnum #{const SDLK_PRIOR} = Prior
  toEnum #{const SDLK_RETURN2} = Return2
  toEnum #{const SDLK_SEPARATOR} = Separator
  toEnum #{const SDLK_OUT} = Out
  toEnum #{const SDLK_OPER} = Oper
  toEnum #{const SDLK_CLEARAGAIN} = ClearAgain
  toEnum #{const SDLK_CRSEL} = CrSel
  toEnum #{const SDLK_EXSEL} = ExSel
  toEnum #{const SDLK_KP_00} = Keypad00
  toEnum #{const SDLK_KP_000} = Keypad000
  toEnum #{const SDLK_THOUSANDSSEPARATOR} = ThousandSeparator
  toEnum #{const SDLK_DECIMALSEPARATOR} = DecimalSeparator
  toEnum #{const SDLK_CURRENCYUNIT} = CurrencyUnit
  toEnum #{const SDLK_CURRENCYSUBUNIT} = CurrencySubunit
  toEnum #{const SDLK_KP_LEFTPAREN} = KeypadLeftParen
  toEnum #{const SDLK_KP_RIGHTPAREN} = KeypadRightParen
  toEnum #{const SDLK_KP_LEFTBRACE} = KeypadLeftBrace
  toEnum #{const SDLK_KP_RIGHTBRACE} = KeypadRightBrace
  toEnum #{const SDLK_KP_TAB} = KeypadTab
  toEnum #{const SDLK_KP_BACKSPACE} = KeypadBackspace
  toEnum #{const SDLK_KP_A} = KeypadA
  toEnum #{const SDLK_KP_B} = KeypadB
  toEnum #{const SDLK_KP_C} = KeypadC
  toEnum #{const SDLK_KP_D} = KeypadD
  toEnum #{const SDLK_KP_E} = KeypadE
  toEnum #{const SDLK_KP_F} = KeypadF
  toEnum #{const SDLK_KP_XOR} = KeypadXOR
  toEnum #{const SDLK_KP_POWER} = KeypadPower
  toEnum #{const SDLK_KP_PERCENT} = KeypadPercent
  toEnum #{const SDLK_KP_LESS} = KeypadLess
  toEnum #{const SDLK_KP_GREATER} = KeypadGreater
  toEnum #{const SDLK_KP_AMPERSAND} = KeypadAmpersand
  toEnum #{const SDLK_KP_DBLAMPERSAND} = KeypadDoubleAmpersand
  toEnum #{const SDLK_KP_VERTICALBAR} = KeypadVerticalBar
  toEnum #{const SDLK_KP_DBLVERTICALBAR} = KeypadDoubleVerticalBar
  toEnum #{const SDLK_KP_COLON} = KeypadColon
  toEnum #{const SDLK_KP_HASH} = KeypadHash
  toEnum #{const SDLK_KP_SPACE} = KeypadSpace
  toEnum #{const SDLK_KP_AT} = KeypadAt
  toEnum #{const SDLK_KP_EXCLAM} = KeypadExclamation
  toEnum #{const SDLK_KP_MEMSTORE} = KeypadMemStore
  toEnum #{const SDLK_KP_MEMRECALL} = KeypadMemRecall
  toEnum #{const SDLK_KP_MEMCLEAR} = KeypadMemClear
  toEnum #{const SDLK_KP_MEMADD} = KeypadMemAdd
  toEnum #{const SDLK_KP_MEMSUBTRACT} = KeypadMemSubstract
  toEnum #{const SDLK_KP_MEMMULTIPLY} = KeypadMemMultiply
  toEnum #{const SDLK_KP_MEMDIVIDE} = KeypadMemDivide
  toEnum #{const SDLK_KP_PLUSMINUS} = KeypadPlusMinus
  toEnum #{const SDLK_KP_CLEAR} = KeypadClear
  toEnum #{const SDLK_KP_CLEARENTRY} = KeypadClearEntry
  toEnum #{const SDLK_KP_BINARY} = KeypadBinary
  toEnum #{const SDLK_KP_OCTAL} = KeypadOctal
  toEnum #{const SDLK_KP_DECIMAL} = KeypadDecimal
  toEnum #{const SDLK_KP_HEXADECIMAL} = KeypadHexadecimal
  toEnum #{const SDLK_LCTRL} = LeftControl
  toEnum #{const SDLK_LSHIFT} = LeftShift
  toEnum #{const SDLK_LALT} = LeftAlt
  toEnum #{const SDLK_LGUI} = LeftGUI
  toEnum #{const SDLK_RCTRL} = RightControl
  toEnum #{const SDLK_RSHIFT} = RightShift
  toEnum #{const SDLK_RALT} = RightAlt
  toEnum #{const SDLK_RGUI} = RightGUI
  toEnum #{const SDLK_MODE} = Mode
  toEnum #{const SDLK_AUDIONEXT} = AudioNext
  toEnum #{const SDLK_AUDIOPREV} = AudioPrevious
  toEnum #{const SDLK_AUDIOSTOP} = AudioStop
  toEnum #{const SDLK_AUDIOPLAY} = AudioPlay
  toEnum #{const SDLK_AUDIOMUTE} = AudioMute
  toEnum #{const SDLK_MEDIASELECT} = MediaSelect
  toEnum #{const SDLK_WWW} = WWW
  toEnum #{const SDLK_MAIL} = Mail
  toEnum #{const SDLK_CALCULATOR} = Calculator
  toEnum #{const SDLK_COMPUTER} = Computer
  toEnum #{const SDLK_AC_SEARCH} = ACSearch
  toEnum #{const SDLK_AC_HOME} = ACHome
  toEnum #{const SDLK_AC_BACK} = ACBack
  toEnum #{const SDLK_AC_FORWARD} = ACForward
  toEnum #{const SDLK_AC_STOP} = ACStop
  toEnum #{const SDLK_AC_REFRESH} = ACRefresh
  toEnum #{const SDLK_AC_BOOKMARKS} = ACBookmarks
  toEnum #{const SDLK_BRIGHTNESSDOWN} = BrightnessDown
  toEnum #{const SDLK_BRIGHTNESSUP} = BrightnessUp
  toEnum #{const SDLK_DISPLAYSWITCH} = DisplaySwitch
  toEnum #{const SDLK_KBDILLUMTOGGLE} = KBIllumToggle
  toEnum #{const SDLK_KBDILLUMDOWN} = KBIllumDown
  toEnum #{const SDLK_KBDILLUMUP} = KBIllumUp
  toEnum #{const SDLK_EJECT} = Eject
  toEnum #{const SDLK_SLEEP} = Sleep
  toEnum #{const SDLK_AMPERSAND} = Ampersand
  toEnum #{const SDLK_ASTERISK} = Asterisk
  toEnum #{const SDLK_AT} = At
  toEnum #{const SDLK_CARET} = Caret
  toEnum #{const SDLK_COLON} = Colon
  toEnum #{const SDLK_DOLLAR} = Dollar
  toEnum #{const SDLK_EXCLAIM} = Exclaim
  toEnum #{const SDLK_GREATER} = Greater
  toEnum #{const SDLK_HASH} = Hash
  toEnum #{const SDLK_LEFTPAREN} = LeftParen
  toEnum #{const SDLK_LESS} = Less
  toEnum #{const SDLK_PERCENT} = Percent
  toEnum #{const SDLK_PLUS} = Plus
  toEnum #{const SDLK_QUESTION} = Question
  toEnum #{const SDLK_QUOTEDBL} = DoubleQuote
  toEnum #{const SDLK_RIGHTPAREN} = RightParen
  toEnum #{const SDLK_UNDERSCORE} = Underscore
  toEnum _ = error "Keycode.toEnum: Invalid argument."

  fromEnum A = #{const SDLK_a}
  fromEnum B = #{const SDLK_b}
  fromEnum C = #{const SDLK_c}
  fromEnum D = #{const SDLK_d}
  fromEnum E = #{const SDLK_e}
  fromEnum F = #{const SDLK_f}
  fromEnum G = #{const SDLK_g}
  fromEnum H = #{const SDLK_h}
  fromEnum I = #{const SDLK_i}
  fromEnum J = #{const SDLK_j}
  fromEnum K = #{const SDLK_k}
  fromEnum L = #{const SDLK_l}
  fromEnum M = #{const SDLK_m}
  fromEnum N = #{const SDLK_n}
  fromEnum O = #{const SDLK_o}
  fromEnum P = #{const SDLK_p}
  fromEnum Q = #{const SDLK_q}
  fromEnum R = #{const SDLK_r}
  fromEnum S = #{const SDLK_s}
  fromEnum T = #{const SDLK_t}
  fromEnum U = #{const SDLK_u}
  fromEnum V = #{const SDLK_v}
  fromEnum W = #{const SDLK_w}
  fromEnum X = #{const SDLK_x}
  fromEnum Y = #{const SDLK_y}
  fromEnum Z = #{const SDLK_z}
  fromEnum Number1 = #{const SDLK_1}
  fromEnum Number2 = #{const SDLK_2}
  fromEnum Number3 = #{const SDLK_3}
  fromEnum Number4 = #{const SDLK_4}
  fromEnum Number5 = #{const SDLK_5}
  fromEnum Number6 = #{const SDLK_6}
  fromEnum Number7 = #{const SDLK_7}
  fromEnum Number8 = #{const SDLK_8}
  fromEnum Number9 = #{const SDLK_9}
  fromEnum Number0 = #{const SDLK_0}
  fromEnum Return = #{const SDLK_RETURN}
  fromEnum Escape = #{const SDLK_ESCAPE}
  fromEnum Backspace = #{const SDLK_BACKSPACE}
  fromEnum Tab = #{const SDLK_TAB}
  fromEnum Space = #{const SDLK_SPACE}
  fromEnum Minus = #{const SDLK_MINUS}
  fromEnum Equals = #{const SDLK_EQUALS}
  fromEnum LeftBracket = #{const SDLK_LEFTBRACKET}
  fromEnum RightBracket = #{const SDLK_RIGHTBRACKET}
  fromEnum Backslash = #{const SDLK_BACKSLASH}
  fromEnum Semicolon = #{const SDLK_SEMICOLON}
  fromEnum Comma = #{const SDLK_COMMA}
  fromEnum Period = #{const SDLK_PERIOD}
  fromEnum Slash = #{const SDLK_SLASH}
  fromEnum Capslock = #{const SDLK_CAPSLOCK}
  fromEnum F1 = #{const SDLK_F1}
  fromEnum F2 = #{const SDLK_F2}
  fromEnum F3 = #{const SDLK_F3}
  fromEnum F4 = #{const SDLK_F4}
  fromEnum F5 = #{const SDLK_F5}
  fromEnum F6 = #{const SDLK_F6}
  fromEnum F7 = #{const SDLK_F7}
  fromEnum F8 = #{const SDLK_F8}
  fromEnum F9 = #{const SDLK_F9}
  fromEnum F10 = #{const SDLK_F10}
  fromEnum F11 = #{const SDLK_F11}
  fromEnum F12 = #{const SDLK_F12}
  fromEnum PrintScreen = #{const SDLK_PRINTSCREEN}
  fromEnum ScrollLock = #{const SDLK_SCROLLLOCK}
  fromEnum Pause = #{const SDLK_PAUSE}
  fromEnum Insert = #{const SDLK_INSERT}
  fromEnum Home = #{const SDLK_HOME}
  fromEnum PageUp = #{const SDLK_PAGEUP}
  fromEnum Delete = #{const SDLK_DELETE}
  fromEnum End = #{const SDLK_END}
  fromEnum PageDown = #{const SDLK_PAGEDOWN}
  fromEnum Right = #{const SDLK_RIGHT}
  fromEnum Left = #{const SDLK_LEFT}
  fromEnum Down = #{const SDLK_DOWN}
  fromEnum Up = #{const SDLK_UP}
  fromEnum NumLockClear = #{const SDLK_NUMLOCKCLEAR}
  fromEnum KeypadDivide = #{const SDLK_KP_DIVIDE}
  fromEnum KeypadMultiply = #{const SDLK_KP_MULTIPLY}
  fromEnum KeypadMinus = #{const SDLK_KP_MINUS}
  fromEnum KeypadPlus = #{const SDLK_KP_PLUS}
  fromEnum KeypadEnter = #{const SDLK_KP_ENTER}
  fromEnum Keypad1 = #{const SDLK_KP_1}
  fromEnum Keypad2 = #{const SDLK_KP_2}
  fromEnum Keypad3 = #{const SDLK_KP_3}
  fromEnum Keypad4 = #{const SDLK_KP_4}
  fromEnum Keypad5 = #{const SDLK_KP_5}
  fromEnum Keypad6 = #{const SDLK_KP_6}
  fromEnum Keypad7 = #{const SDLK_KP_7}
  fromEnum Keypad8 = #{const SDLK_KP_8}
  fromEnum Keypad9 = #{const SDLK_KP_9}
  fromEnum Keypad0 = #{const SDLK_KP_0}
  fromEnum KeypadPeriod = #{const SDLK_KP_PERIOD}
  fromEnum Application = #{const SDLK_APPLICATION}
  fromEnum Power = #{const SDLK_POWER}
  fromEnum KeypadEquals = #{const SDLK_KP_EQUALS}
  fromEnum F13 = #{const SDLK_F13}
  fromEnum F14 = #{const SDLK_F14}
  fromEnum F15 = #{const SDLK_F15}
  fromEnum F16 = #{const SDLK_F16}
  fromEnum F17 = #{const SDLK_F17}
  fromEnum F18 = #{const SDLK_F18}
  fromEnum F19 = #{const SDLK_F19}
  fromEnum F20 = #{const SDLK_F20}
  fromEnum F21 = #{const SDLK_F21}
  fromEnum F22 = #{const SDLK_F22}
  fromEnum F23 = #{const SDLK_F23}
  fromEnum F24 = #{const SDLK_F24}
  fromEnum Execute = #{const SDLK_EXECUTE}
  fromEnum Help = #{const SDLK_HELP}
  fromEnum Menu = #{const SDLK_MENU}
  fromEnum Select = #{const SDLK_SELECT}
  fromEnum Stop = #{const SDLK_STOP}
  fromEnum Again = #{const SDLK_AGAIN}
  fromEnum Undo = #{const SDLK_UNDO}
  fromEnum Cut = #{const SDLK_CUT}
  fromEnum Copy = #{const SDLK_COPY}
  fromEnum Paste = #{const SDLK_PASTE}
  fromEnum Find = #{const SDLK_FIND}
  fromEnum Mute = #{const SDLK_MUTE}
  fromEnum VolumeUp = #{const SDLK_VOLUMEUP}
  fromEnum VolumeDown = #{const SDLK_VOLUMEDOWN}
  fromEnum KeypadComma = #{const SDLK_KP_COMMA}
  fromEnum KeyPadEqualsAs400 = #{const SDLK_KP_EQUALSAS400}
  fromEnum AltErase = #{const SDLK_ALTERASE}
  fromEnum SysReq = #{const SDLK_SYSREQ}
  fromEnum Cancel = #{const SDLK_CANCEL}
  fromEnum Clear = #{const SDLK_CLEAR}
  fromEnum Prior = #{const SDLK_PRIOR}
  fromEnum Return2 = #{const SDLK_RETURN2}
  fromEnum Separator = #{const SDLK_SEPARATOR}
  fromEnum Out = #{const SDLK_OUT}
  fromEnum Oper = #{const SDLK_OPER}
  fromEnum ClearAgain = #{const SDLK_CLEARAGAIN}
  fromEnum CrSel = #{const SDLK_CRSEL}
  fromEnum ExSel = #{const SDLK_EXSEL}
  fromEnum Keypad00 = #{const SDLK_KP_00}
  fromEnum Keypad000 = #{const SDLK_KP_000}
  fromEnum ThousandSeparator = #{const SDLK_THOUSANDSSEPARATOR}
  fromEnum DecimalSeparator = #{const SDLK_DECIMALSEPARATOR}
  fromEnum CurrencyUnit = #{const SDLK_CURRENCYUNIT}
  fromEnum CurrencySubunit = #{const SDLK_CURRENCYSUBUNIT}
  fromEnum KeypadLeftParen = #{const SDLK_KP_LEFTPAREN}
  fromEnum KeypadRightParen = #{const SDLK_KP_RIGHTPAREN}
  fromEnum KeypadLeftBrace = #{const SDLK_KP_LEFTBRACE}
  fromEnum KeypadRightBrace = #{const SDLK_KP_RIGHTBRACE}
  fromEnum KeypadTab = #{const SDLK_KP_TAB}
  fromEnum KeypadBackspace = #{const SDLK_KP_BACKSPACE}
  fromEnum KeypadA = #{const SDLK_KP_A}
  fromEnum KeypadB = #{const SDLK_KP_B}
  fromEnum KeypadC = #{const SDLK_KP_C}
  fromEnum KeypadD = #{const SDLK_KP_D}
  fromEnum KeypadE = #{const SDLK_KP_E}
  fromEnum KeypadF = #{const SDLK_KP_F}
  fromEnum KeypadXOR = #{const SDLK_KP_XOR}
  fromEnum KeypadPower = #{const SDLK_KP_POWER}
  fromEnum KeypadPercent = #{const SDLK_KP_PERCENT}
  fromEnum KeypadLess = #{const SDLK_KP_LESS}
  fromEnum KeypadGreater = #{const SDLK_KP_GREATER}
  fromEnum KeypadAmpersand = #{const SDLK_KP_AMPERSAND}
  fromEnum KeypadDoubleAmpersand = #{const SDLK_KP_DBLAMPERSAND}
  fromEnum KeypadVerticalBar = #{const SDLK_KP_VERTICALBAR}
  fromEnum KeypadDoubleVerticalBar = #{const SDLK_KP_DBLVERTICALBAR}
  fromEnum KeypadColon = #{const SDLK_KP_COLON}
  fromEnum KeypadHash = #{const SDLK_KP_HASH}
  fromEnum KeypadSpace = #{const SDLK_KP_SPACE}
  fromEnum KeypadAt = #{const SDLK_KP_AT}
  fromEnum KeypadExclamation = #{const SDLK_KP_EXCLAM}
  fromEnum KeypadMemStore = #{const SDLK_KP_MEMSTORE}
  fromEnum KeypadMemRecall = #{const SDLK_KP_MEMRECALL}
  fromEnum KeypadMemClear = #{const SDLK_KP_MEMCLEAR}
  fromEnum KeypadMemAdd = #{const SDLK_KP_MEMADD}
  fromEnum KeypadMemSubstract = #{const SDLK_KP_MEMSUBTRACT}
  fromEnum KeypadMemMultiply = #{const SDLK_KP_MEMMULTIPLY}
  fromEnum KeypadMemDivide = #{const SDLK_KP_MEMDIVIDE}
  fromEnum KeypadPlusMinus = #{const SDLK_KP_PLUSMINUS}
  fromEnum KeypadClear = #{const SDLK_KP_CLEAR}
  fromEnum KeypadClearEntry = #{const SDLK_KP_CLEARENTRY}
  fromEnum KeypadBinary = #{const SDLK_KP_BINARY}
  fromEnum KeypadOctal = #{const SDLK_KP_OCTAL}
  fromEnum KeypadDecimal = #{const SDLK_KP_DECIMAL}
  fromEnum KeypadHexadecimal = #{const SDLK_KP_HEXADECIMAL}
  fromEnum LeftControl = #{const SDLK_LCTRL}
  fromEnum LeftShift = #{const SDLK_LSHIFT}
  fromEnum LeftAlt = #{const SDLK_LALT}
  fromEnum LeftGUI = #{const SDLK_LGUI}
  fromEnum RightControl = #{const SDLK_RCTRL}
  fromEnum RightShift = #{const SDLK_RSHIFT}
  fromEnum RightAlt = #{const SDLK_RALT}
  fromEnum RightGUI = #{const SDLK_RGUI}
  fromEnum Mode = #{const SDLK_MODE}
  fromEnum AudioNext = #{const SDLK_AUDIONEXT}
  fromEnum AudioPrevious = #{const SDLK_AUDIOPREV}
  fromEnum AudioStop = #{const SDLK_AUDIOSTOP}
  fromEnum AudioPlay = #{const SDLK_AUDIOPLAY}
  fromEnum AudioMute = #{const SDLK_AUDIOMUTE}
  fromEnum MediaSelect = #{const SDLK_MEDIASELECT}
  fromEnum WWW = #{const SDLK_WWW}
  fromEnum Mail = #{const SDLK_MAIL}
  fromEnum Calculator = #{const SDLK_CALCULATOR}
  fromEnum Computer = #{const SDLK_COMPUTER}
  fromEnum ACSearch = #{const SDLK_AC_SEARCH}
  fromEnum ACHome = #{const SDLK_AC_HOME}
  fromEnum ACBack = #{const SDLK_AC_BACK}
  fromEnum ACForward = #{const SDLK_AC_FORWARD}
  fromEnum ACStop = #{const SDLK_AC_STOP}
  fromEnum ACRefresh = #{const SDLK_AC_REFRESH}
  fromEnum ACBookmarks = #{const SDLK_AC_BOOKMARKS}
  fromEnum BrightnessDown = #{const SDLK_BRIGHTNESSDOWN}
  fromEnum BrightnessUp = #{const SDLK_BRIGHTNESSUP}
  fromEnum DisplaySwitch = #{const SDLK_DISPLAYSWITCH}
  fromEnum KBIllumToggle = #{const SDLK_KBDILLUMTOGGLE}
  fromEnum KBIllumDown = #{const SDLK_KBDILLUMDOWN}
  fromEnum KBIllumUp = #{const SDLK_KBDILLUMUP}
  fromEnum Eject = #{const SDLK_EJECT}
  fromEnum Sleep = #{const SDLK_SLEEP}
  fromEnum Ampersand = #{const SDLK_AMPERSAND}
  fromEnum Asterisk = #{const SDLK_ASTERISK}
  fromEnum At = #{const SDLK_AT}
  fromEnum Caret = #{const SDLK_CARET}
  fromEnum Colon = #{const SDLK_COLON}
  fromEnum Dollar = #{const SDLK_DOLLAR}
  fromEnum Exclaim = #{const SDLK_EXCLAIM}
  fromEnum Greater = #{const SDLK_GREATER}
  fromEnum Hash = #{const SDLK_HASH}
  fromEnum LeftParen = #{const SDLK_LEFTPAREN}
  fromEnum Less = #{const SDLK_LESS}
  fromEnum Percent = #{const SDLK_PERCENT}
  fromEnum Plus = #{const SDLK_PLUS}
  fromEnum Question = #{const SDLK_QUESTION}
  fromEnum DoubleQuote = #{const SDLK_QUOTEDBL}
  fromEnum RightParen = #{const SDLK_RIGHTPAREN}
  fromEnum Underscore = #{const SDLK_UNDERSCORE}
