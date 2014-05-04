#include "SDL.h"
module Graphics.UI.SDL.Keyboard
  ( Keymod(..)
  , Scancode(..)
  , getKeyboardFocus
  , getModState
  , setModState
  , getKeyFromScancode
  , getScancodeFromKey
  , getScancodeName
  , getScancodeFromName
  , getKeyName
  , getKeyFromName
  , startTextInput
  , isTextInputActive
  , stopTextInput
  , setTextInputRect
  , hasScreenKeyboardSupport
  , isScreenKeyboardShown
  ) where

import Foreign
import Foreign.C.String
import Control.Applicative
import Graphics.UI.SDL.Types
import Graphics.UI.SDL.Rect
import Graphics.UI.SDL.Utilities
import Graphics.UI.SDL.Raw
import Graphics.UI.SDL.Keycode

data Scancode
   = ScancodeUNKNOWN
   -- alpha
   | ScancodeA | ScancodeB | ScancodeC | ScancodeD | ScancodeE | ScancodeF
   | ScancodeG | ScancodeH | ScancodeI | ScancodeJ | ScancodeK | ScancodeL
   | ScancodeM | ScancodeN | ScancodeO | ScancodeP | ScancodeQ | ScancodeR
   | ScancodeS | ScancodeT | ScancodeU | ScancodeV | ScancodeW | ScancodeX
   | ScancodeY | ScancodeZ
   -- numeric
   | Scancode1 | Scancode2 | Scancode3 | Scancode4 | Scancode5 | Scancode6
   | Scancode7 | Scancode8 | Scancode9 | Scancode0
   -- notation
   | ScancodeReturn | ScancodeEscape | ScancodeBackspace | ScancodeTab
   | ScancodeSpace | ScancodeMinus | ScancodeEquals | ScancodeLeftbracket
   | ScancodeRightbracket | ScancodeBackslash | ScancodeNonushash
   | ScancodeSemicolon | ScancodeApostrophe | ScancodeGrave | ScancodeComma
   | ScancodePeriod | ScancodeSlash | ScancodeCapslock
   -- function keys
   | ScancodeF1 | ScancodeF2 | ScancodeF3 | ScancodeF4 | ScancodeF5
   | ScancodeF6 | ScancodeF7 | ScancodeF8 | ScancodeF9 | ScancodeF10
   | ScancodeF11 | ScancodeF12
   -- shortcut keys
   | ScancodePrintscreen | ScancodeScrolllock | ScancodePause | ScancodeInsert
   | ScancodeHome | ScancodePageup | ScancodeDelete | ScancodeEnd
   | ScancodePagedown | ScancodeRight | ScancodeLeft | ScancodeDown | ScancodeUp
   | ScancodeNumlockclear | ScancodeKPDivide | ScancodeKPMultiply
   | ScancodeKPMinus | ScancodeKPplus | ScancodeKPEnter
   | ScancodeKP1 | ScancodeKP2 | ScancodeKP3 | ScancodeKP4 | ScancodeKP5
   | ScancodeKP6 | ScancodeKP7 | ScancodeKP8 | ScancodeKP9 | ScancodeKP0
   | ScancodeKPPeriod | ScancodeNonusbackslash | ScancodeApplication
   | ScancodePower | ScancodeKPEquals
   -- extended function keys
   | ScancodeF13 | ScancodeF14 | ScancodeF15 | ScancodeF16 | ScancodeF17
   | ScancodeF18 | ScancodeF19 | ScancodeF20 | ScancodeF21 | ScancodeF22
   | ScancodeF23 | ScancodeF24
   -- extended shortcut keys
   | ScancodeExecute | ScancodeHelp | ScancodeMenu | ScancodeSelect
   | ScancodeStop | ScancodeAgain | ScancodeUndo | ScancodeCut | ScancodeCopy
   | ScancodePaste | ScancodeFind | ScancodeMute | ScancodeVolumeup
   | ScancodeVolumedown | ScancodeKPComma | ScancodeKPEqualsas400
   -- international
   | ScancodeInternational1 | ScancodeInternational2 | ScancodeInternational3
   | ScancodeInternational4 | ScancodeInternational5 | ScancodeInternational6
   | ScancodeInternational7 | ScancodeInternational8 | ScancodeInternational9
   -- languages
   | ScancodeLang1 | ScancodeLang2 | ScancodeLang3 | ScancodeLang4
   | ScancodeLang5 | ScancodeLang6 | ScancodeLang7 | ScancodeLang8
   | ScancodeLang9
   -- keypad/calc
   | ScancodeAlterase | ScancodeSysreq | ScancodeCancel | ScancodeClear
   | ScancodePrior | ScancodeReturn2 | ScancodeSeparator | ScancodeOut
   | ScancodeOper | ScancodeClearagain | ScancodeCrsel | ScancodeExsel
   | ScancodeKP00 | ScancodeKP000
   | ScancodeThousandsseparator | ScancodeDecimalseparator | ScancodeCurrencyunit
   | ScancodeCurrencysubunit | ScancodeKPLeftparen | ScancodeKPRightparen
   | ScancodeKPLeftbrace | ScancodeKPRightbrace | ScancodeKPTab | ScancodeKPBackspace
   | ScancodeKPA | ScancodeKPB | ScancodeKPC | ScancodeKPD | ScancodeKPE | ScancodeKPF
   | ScancodeKPXor | ScancodeKPPower | ScancodeKPPercent | ScancodeKPLess
   | ScancodeKPGreater | ScancodeKPAmpersand | ScancodeKPDblampersand
   | ScancodeKPVerticalbar | ScancodeKPDblverticalbar | ScancodeKPColon
   | ScancodeKPHash | ScancodeKPSpace | ScancodeKPAt | ScancodeKPExclam
   | ScancodeKPMemstore | ScancodeKPMemrecall | ScancodeKPMemclear | ScancodeKPMemadd
   | ScancodeKPMemsubtract | ScancodeKPMemmultiply | ScancodeKPMemdivide
   | ScancodeKPPlusminus | ScancodeKPClear | ScancodeKPClearentry | ScancodeKPBinary
   | ScancodeKPOctal | ScancodeKPDecimal | ScancodeKPHexadecimal
   | ScancodeLCtrl | ScancodeLShift | ScancodeLAlt | ScancodeLGUI | ScancodeRCtrl
   | ScancodeRShift | ScancodeRAlt | ScancodeRGUI | ScancodeMode
   -- audio/media keys
   | ScancodeAudionext | ScancodeAudioprev | ScancodeAudiostop
   | ScancodeAudioplay | ScancodeAudiomute | ScancodeMediaselect
   | ScancodeWWW | ScancodeMail | ScancodeCalculator | ScancodeComputer
   | ScancodeACSearch | ScancodeACHome | ScancodeACBack
   | ScancodeACForward | ScancodeACStop | ScancodeACRefresh | ScancodeACBookmarks
   -- mac
   | ScancodeBrightnessDown | ScancodeBrightnessUp | ScancodeDisplaySwitch
   | ScancodeKBDILLUMTOGGLE | ScancodeKBDIlluMDown | ScancodeKBDIllumUp
   | ScancodeEject | ScancodeSleep | ScancodeApp1 | ScancodeApp2
   deriving (Eq, Show)

instance Enum Scancode where
  fromEnum x =
    case x of
      ScancodeUNKNOWN -> #{const SDL_SCANCODE_UNKNOWN}
      ScancodeA -> #{const SDL_SCANCODE_A}
      ScancodeB -> #{const SDL_SCANCODE_B}
      ScancodeC -> #{const SDL_SCANCODE_C}
      ScancodeD -> #{const SDL_SCANCODE_D}
      ScancodeE -> #{const SDL_SCANCODE_E}
      ScancodeF -> #{const SDL_SCANCODE_F}
      ScancodeG -> #{const SDL_SCANCODE_G}
      ScancodeH -> #{const SDL_SCANCODE_H}
      ScancodeI -> #{const SDL_SCANCODE_I}
      ScancodeJ -> #{const SDL_SCANCODE_J}
      ScancodeK -> #{const SDL_SCANCODE_K}
      ScancodeL -> #{const SDL_SCANCODE_L}
      ScancodeM -> #{const SDL_SCANCODE_M}
      ScancodeN -> #{const SDL_SCANCODE_N}
      ScancodeO -> #{const SDL_SCANCODE_O}
      ScancodeP -> #{const SDL_SCANCODE_P}
      ScancodeQ -> #{const SDL_SCANCODE_Q}
      ScancodeR -> #{const SDL_SCANCODE_R}
      ScancodeS -> #{const SDL_SCANCODE_S}
      ScancodeT -> #{const SDL_SCANCODE_T}
      ScancodeU -> #{const SDL_SCANCODE_U}
      ScancodeV -> #{const SDL_SCANCODE_V}
      ScancodeW -> #{const SDL_SCANCODE_W}
      ScancodeX -> #{const SDL_SCANCODE_X}
      ScancodeY -> #{const SDL_SCANCODE_Y}
      ScancodeZ -> #{const SDL_SCANCODE_Z}
      Scancode1 -> #{const SDL_SCANCODE_1}
      Scancode2 -> #{const SDL_SCANCODE_2}
      Scancode3 -> #{const SDL_SCANCODE_3}
      Scancode4 -> #{const SDL_SCANCODE_4}
      Scancode5 -> #{const SDL_SCANCODE_5}
      Scancode6 -> #{const SDL_SCANCODE_6}
      Scancode7 -> #{const SDL_SCANCODE_7}
      Scancode8 -> #{const SDL_SCANCODE_8}
      Scancode9 -> #{const SDL_SCANCODE_9}
      Scancode0 -> #{const SDL_SCANCODE_0}
      ScancodeReturn -> #{const SDL_SCANCODE_RETURN}
      ScancodeEscape -> #{const SDL_SCANCODE_ESCAPE}
      ScancodeBackspace -> #{const SDL_SCANCODE_BACKSPACE}
      ScancodeTab -> #{const SDL_SCANCODE_TAB}
      ScancodeSpace -> #{const SDL_SCANCODE_SPACE}
      ScancodeMinus -> #{const SDL_SCANCODE_MINUS}
      ScancodeEquals -> #{const SDL_SCANCODE_EQUALS}
      ScancodeLeftbracket -> #{const SDL_SCANCODE_LEFTBRACKET}
      ScancodeRightbracket -> #{const SDL_SCANCODE_RIGHTBRACKET}
      ScancodeBackslash -> #{const SDL_SCANCODE_BACKSLASH}
      ScancodeNonushash -> #{const SDL_SCANCODE_NONUSHASH}
      ScancodeSemicolon -> #{const SDL_SCANCODE_SEMICOLON}
      ScancodeApostrophe -> #{const SDL_SCANCODE_APOSTROPHE}
      ScancodeGrave -> #{const SDL_SCANCODE_GRAVE}
      ScancodeComma -> #{const SDL_SCANCODE_COMMA}
      ScancodePeriod -> #{const SDL_SCANCODE_PERIOD}
      ScancodeSlash -> #{const SDL_SCANCODE_SLASH}
      ScancodeCapslock -> #{const SDL_SCANCODE_CAPSLOCK}
      ScancodeF1 -> #{const SDL_SCANCODE_F1}
      ScancodeF2 -> #{const SDL_SCANCODE_F2}
      ScancodeF3 -> #{const SDL_SCANCODE_F3}
      ScancodeF4 -> #{const SDL_SCANCODE_F4}
      ScancodeF5 -> #{const SDL_SCANCODE_F5}
      ScancodeF6 -> #{const SDL_SCANCODE_F6}
      ScancodeF7 -> #{const SDL_SCANCODE_F7}
      ScancodeF8 -> #{const SDL_SCANCODE_F8}
      ScancodeF9 -> #{const SDL_SCANCODE_F9}
      ScancodeF10 -> #{const SDL_SCANCODE_F10}
      ScancodeF11 -> #{const SDL_SCANCODE_F11}
      ScancodeF12 -> #{const SDL_SCANCODE_F12}
      ScancodePrintscreen -> #{const SDL_SCANCODE_PRINTSCREEN}
      ScancodeScrolllock -> #{const SDL_SCANCODE_SCROLLLOCK}
      ScancodePause -> #{const SDL_SCANCODE_PAUSE}
      ScancodeInsert -> #{const SDL_SCANCODE_INSERT}
      ScancodeHome -> #{const SDL_SCANCODE_HOME}
      ScancodePageup -> #{const SDL_SCANCODE_PAGEUP}
      ScancodeDelete -> #{const SDL_SCANCODE_DELETE}
      ScancodeEnd -> #{const SDL_SCANCODE_END}
      ScancodePagedown -> #{const SDL_SCANCODE_PAGEDOWN}
      ScancodeRight -> #{const SDL_SCANCODE_RIGHT}
      ScancodeLeft -> #{const SDL_SCANCODE_LEFT}
      ScancodeDown -> #{const SDL_SCANCODE_DOWN}
      ScancodeUp -> #{const SDL_SCANCODE_UP}
      ScancodeNumlockclear -> #{const SDL_SCANCODE_NUMLOCKCLEAR}
      ScancodeKPDivide -> #{const SDL_SCANCODE_KP_DIVIDE}
      ScancodeKPMultiply -> #{const SDL_SCANCODE_KP_MULTIPLY}
      ScancodeKPMinus -> #{const SDL_SCANCODE_KP_MINUS}
      ScancodeKPplus -> #{const SDL_SCANCODE_KP_PLUS}
      ScancodeKPEnter -> #{const SDL_SCANCODE_KP_ENTER}
      ScancodeKP1 -> #{const SDL_SCANCODE_KP_1}
      ScancodeKP2 -> #{const SDL_SCANCODE_KP_2}
      ScancodeKP3 -> #{const SDL_SCANCODE_KP_3}
      ScancodeKP4 -> #{const SDL_SCANCODE_KP_4}
      ScancodeKP5 -> #{const SDL_SCANCODE_KP_5}
      ScancodeKP6 -> #{const SDL_SCANCODE_KP_6}
      ScancodeKP7 -> #{const SDL_SCANCODE_KP_7}
      ScancodeKP8 -> #{const SDL_SCANCODE_KP_8}
      ScancodeKP9 -> #{const SDL_SCANCODE_KP_9}
      ScancodeKP0 -> #{const SDL_SCANCODE_KP_0}
      ScancodeKPPeriod -> #{const SDL_SCANCODE_KP_PERIOD}
      ScancodeNonusbackslash -> #{const SDL_SCANCODE_NONUSBACKSLASH}
      ScancodeApplication -> #{const SDL_SCANCODE_APPLICATION}
      ScancodePower -> #{const SDL_SCANCODE_POWER}
      ScancodeKPEquals -> #{const SDL_SCANCODE_KP_EQUALS}
      ScancodeF13 -> #{const SDL_SCANCODE_F13}
      ScancodeF14 -> #{const SDL_SCANCODE_F14}
      ScancodeF15 -> #{const SDL_SCANCODE_F15}
      ScancodeF16 -> #{const SDL_SCANCODE_F16}
      ScancodeF17 -> #{const SDL_SCANCODE_F17}
      ScancodeF18 -> #{const SDL_SCANCODE_F18}
      ScancodeF19 -> #{const SDL_SCANCODE_F19}
      ScancodeF20 -> #{const SDL_SCANCODE_F20}
      ScancodeF21 -> #{const SDL_SCANCODE_F21}
      ScancodeF22 -> #{const SDL_SCANCODE_F22}
      ScancodeF23 -> #{const SDL_SCANCODE_F23}
      ScancodeF24 -> #{const SDL_SCANCODE_F24}
      ScancodeExecute -> #{const SDL_SCANCODE_EXECUTE}
      ScancodeHelp -> #{const SDL_SCANCODE_HELP}
      ScancodeMenu -> #{const SDL_SCANCODE_MENU}
      ScancodeSelect -> #{const SDL_SCANCODE_SELECT}
      ScancodeStop -> #{const SDL_SCANCODE_STOP}
      ScancodeAgain -> #{const SDL_SCANCODE_AGAIN}
      ScancodeUndo -> #{const SDL_SCANCODE_UNDO}
      ScancodeCut -> #{const SDL_SCANCODE_CUT}
      ScancodeCopy -> #{const SDL_SCANCODE_COPY}
      ScancodePaste -> #{const SDL_SCANCODE_PASTE}
      ScancodeFind -> #{const SDL_SCANCODE_FIND}
      ScancodeMute -> #{const SDL_SCANCODE_MUTE}
      ScancodeVolumeup -> #{const SDL_SCANCODE_VOLUMEUP}
      ScancodeVolumedown -> #{const SDL_SCANCODE_VOLUMEDOWN}
      ScancodeKPComma -> #{const SDL_SCANCODE_KP_COMMA}
      ScancodeKPEqualsas400 -> #{const SDL_SCANCODE_KP_EQUALSAS400}
      ScancodeInternational1 -> #{const SDL_SCANCODE_INTERNATIONAL1}
      ScancodeInternational2 -> #{const SDL_SCANCODE_INTERNATIONAL2}
      ScancodeInternational3 -> #{const SDL_SCANCODE_INTERNATIONAL3}
      ScancodeInternational4 -> #{const SDL_SCANCODE_INTERNATIONAL4}
      ScancodeInternational5 -> #{const SDL_SCANCODE_INTERNATIONAL5}
      ScancodeInternational6 -> #{const SDL_SCANCODE_INTERNATIONAL6}
      ScancodeInternational7 -> #{const SDL_SCANCODE_INTERNATIONAL7}
      ScancodeInternational8 -> #{const SDL_SCANCODE_INTERNATIONAL8}
      ScancodeInternational9 -> #{const SDL_SCANCODE_INTERNATIONAL9}
      ScancodeLang1 -> #{const SDL_SCANCODE_LANG1}
      ScancodeLang2 -> #{const SDL_SCANCODE_LANG2}
      ScancodeLang3 -> #{const SDL_SCANCODE_LANG3}
      ScancodeLang4 -> #{const SDL_SCANCODE_LANG4}
      ScancodeLang5 -> #{const SDL_SCANCODE_LANG5}
      ScancodeLang6 -> #{const SDL_SCANCODE_LANG6}
      ScancodeLang7 -> #{const SDL_SCANCODE_LANG7}
      ScancodeLang8 -> #{const SDL_SCANCODE_LANG8}
      ScancodeLang9 -> #{const SDL_SCANCODE_LANG9}
      ScancodeAlterase -> #{const SDL_SCANCODE_ALTERASE}
      ScancodeSysreq -> #{const SDL_SCANCODE_SYSREQ}
      ScancodeCancel -> #{const SDL_SCANCODE_CANCEL}
      ScancodeClear -> #{const SDL_SCANCODE_CLEAR}
      ScancodePrior -> #{const SDL_SCANCODE_PRIOR}
      ScancodeReturn2 -> #{const SDL_SCANCODE_RETURN2}
      ScancodeSeparator -> #{const SDL_SCANCODE_SEPARATOR}
      ScancodeOut -> #{const SDL_SCANCODE_OUT}
      ScancodeOper -> #{const SDL_SCANCODE_OPER}
      ScancodeClearagain -> #{const SDL_SCANCODE_CLEARAGAIN}
      ScancodeCrsel -> #{const SDL_SCANCODE_CRSEL}
      ScancodeExsel -> #{const SDL_SCANCODE_EXSEL}
      ScancodeKP00 -> #{const SDL_SCANCODE_KP_00}
      ScancodeKP000 -> #{const SDL_SCANCODE_KP_000}
      ScancodeThousandsseparator -> #{const SDL_SCANCODE_THOUSANDSSEPARATOR}
      ScancodeDecimalseparator -> #{const SDL_SCANCODE_DECIMALSEPARATOR}
      ScancodeCurrencyunit -> #{const SDL_SCANCODE_CURRENCYUNIT}
      ScancodeCurrencysubunit -> #{const SDL_SCANCODE_CURRENCYSUBUNIT}
      ScancodeKPLeftparen -> #{const SDL_SCANCODE_KP_LEFTPAREN}
      ScancodeKPRightparen -> #{const SDL_SCANCODE_KP_RIGHTPAREN}
      ScancodeKPLeftbrace -> #{const SDL_SCANCODE_KP_LEFTBRACE}
      ScancodeKPRightbrace -> #{const SDL_SCANCODE_KP_RIGHTBRACE}
      ScancodeKPTab -> #{const SDL_SCANCODE_KP_TAB}
      ScancodeKPBackspace -> #{const SDL_SCANCODE_KP_BACKSPACE}
      ScancodeKPA -> #{const SDL_SCANCODE_KP_A}
      ScancodeKPB -> #{const SDL_SCANCODE_KP_B}
      ScancodeKPC -> #{const SDL_SCANCODE_KP_C}
      ScancodeKPD -> #{const SDL_SCANCODE_KP_D}
      ScancodeKPE -> #{const SDL_SCANCODE_KP_E}
      ScancodeKPF -> #{const SDL_SCANCODE_KP_F}
      ScancodeKPXor -> #{const SDL_SCANCODE_KP_XOR}
      ScancodeKPPower -> #{const SDL_SCANCODE_KP_POWER}
      ScancodeKPPercent -> #{const SDL_SCANCODE_KP_PERCENT}
      ScancodeKPLess -> #{const SDL_SCANCODE_KP_LESS}
      ScancodeKPGreater -> #{const SDL_SCANCODE_KP_GREATER}
      ScancodeKPAmpersand -> #{const SDL_SCANCODE_KP_AMPERSAND}
      ScancodeKPDblampersand -> #{const SDL_SCANCODE_KP_DBLAMPERSAND}
      ScancodeKPVerticalbar -> #{const SDL_SCANCODE_KP_VERTICALBAR}
      ScancodeKPDblverticalbar -> #{const SDL_SCANCODE_KP_DBLVERTICALBAR}
      ScancodeKPColon -> #{const SDL_SCANCODE_KP_COLON}
      ScancodeKPHash -> #{const SDL_SCANCODE_KP_HASH}
      ScancodeKPSpace -> #{const SDL_SCANCODE_KP_SPACE}
      ScancodeKPAt -> #{const SDL_SCANCODE_KP_AT}
      ScancodeKPExclam -> #{const SDL_SCANCODE_KP_EXCLAM}
      ScancodeKPMemstore -> #{const SDL_SCANCODE_KP_MEMSTORE}
      ScancodeKPMemrecall -> #{const SDL_SCANCODE_KP_MEMRECALL}
      ScancodeKPMemclear -> #{const SDL_SCANCODE_KP_MEMCLEAR}
      ScancodeKPMemadd -> #{const SDL_SCANCODE_KP_MEMADD}
      ScancodeKPMemsubtract -> #{const SDL_SCANCODE_KP_MEMSUBTRACT}
      ScancodeKPMemmultiply -> #{const SDL_SCANCODE_KP_MEMMULTIPLY}
      ScancodeKPMemdivide -> #{const SDL_SCANCODE_KP_MEMDIVIDE}
      ScancodeKPPlusminus -> #{const SDL_SCANCODE_KP_PLUSMINUS}
      ScancodeKPClear -> #{const SDL_SCANCODE_KP_CLEAR}
      ScancodeKPClearentry -> #{const SDL_SCANCODE_KP_CLEARENTRY}
      ScancodeKPBinary -> #{const SDL_SCANCODE_KP_BINARY}
      ScancodeKPOctal -> #{const SDL_SCANCODE_KP_OCTAL}
      ScancodeKPDecimal -> #{const SDL_SCANCODE_KP_DECIMAL}
      ScancodeKPHexadecimal -> #{const SDL_SCANCODE_KP_HEXADECIMAL}
      ScancodeLCtrl -> #{const SDL_SCANCODE_LCTRL}
      ScancodeLShift -> #{const SDL_SCANCODE_LSHIFT}
      ScancodeLAlt -> #{const SDL_SCANCODE_LALT}
      ScancodeLGUI -> #{const SDL_SCANCODE_LGUI}
      ScancodeRCtrl -> #{const SDL_SCANCODE_RCTRL}
      ScancodeRShift -> #{const SDL_SCANCODE_RSHIFT}
      ScancodeRAlt -> #{const SDL_SCANCODE_RALT}
      ScancodeRGUI -> #{const SDL_SCANCODE_RGUI}
      ScancodeMode -> #{const SDL_SCANCODE_MODE}
      ScancodeAudionext -> #{const SDL_SCANCODE_AUDIONEXT}
      ScancodeAudioprev -> #{const SDL_SCANCODE_AUDIOPREV}
      ScancodeAudiostop -> #{const SDL_SCANCODE_AUDIOSTOP}
      ScancodeAudioplay -> #{const SDL_SCANCODE_AUDIOPLAY}
      ScancodeAudiomute -> #{const SDL_SCANCODE_AUDIOMUTE}
      ScancodeMediaselect -> #{const SDL_SCANCODE_MEDIASELECT}
      ScancodeWWW -> #{const SDL_SCANCODE_WWW}
      ScancodeMail -> #{const SDL_SCANCODE_MAIL}
      ScancodeCalculator -> #{const SDL_SCANCODE_CALCULATOR}
      ScancodeComputer -> #{const SDL_SCANCODE_COMPUTER}
      ScancodeACSearch -> #{const SDL_SCANCODE_AC_SEARCH}
      ScancodeACHome -> #{const SDL_SCANCODE_AC_HOME}
      ScancodeACBack -> #{const SDL_SCANCODE_AC_BACK}
      ScancodeACForward -> #{const SDL_SCANCODE_AC_FORWARD}
      ScancodeACStop -> #{const SDL_SCANCODE_AC_STOP}
      ScancodeACRefresh -> #{const SDL_SCANCODE_AC_REFRESH}
      ScancodeACBookmarks -> #{const SDL_SCANCODE_AC_BOOKMARKS}
      ScancodeBrightnessDown -> #{const SDL_SCANCODE_BRIGHTNESSDOWN}
      ScancodeBrightnessUp -> #{const SDL_SCANCODE_BRIGHTNESSUP}
      ScancodeDisplaySwitch -> #{const SDL_SCANCODE_DISPLAYSWITCH}
      ScancodeKBDILLUMTOGGLE -> #{const SDL_SCANCODE_KBDILLUMTOGGLE}
      ScancodeKBDIlluMDown -> #{const SDL_SCANCODE_KBDILLUMDOWN}
      ScancodeKBDIllumUp -> #{const SDL_SCANCODE_KBDILLUMUP}
      ScancodeEject -> #{const SDL_SCANCODE_EJECT}
      ScancodeSleep -> #{const SDL_SCANCODE_SLEEP}
      ScancodeApp1 -> #{const SDL_SCANCODE_APP1}
      ScancodeApp2 -> #{const SDL_SCANCODE_APP2}

  toEnum x =
    case x of
      #{const SDL_SCANCODE_UNKNOWN} -> ScancodeUNKNOWN
      #{const SDL_SCANCODE_A} -> ScancodeA
      #{const SDL_SCANCODE_B} -> ScancodeB
      #{const SDL_SCANCODE_C} -> ScancodeC
      #{const SDL_SCANCODE_D} -> ScancodeD
      #{const SDL_SCANCODE_E} -> ScancodeE
      #{const SDL_SCANCODE_F} -> ScancodeF
      #{const SDL_SCANCODE_G} -> ScancodeG
      #{const SDL_SCANCODE_H} -> ScancodeH
      #{const SDL_SCANCODE_I} -> ScancodeI
      #{const SDL_SCANCODE_J} -> ScancodeJ
      #{const SDL_SCANCODE_K} -> ScancodeK
      #{const SDL_SCANCODE_L} -> ScancodeL
      #{const SDL_SCANCODE_M} -> ScancodeM
      #{const SDL_SCANCODE_N} -> ScancodeN
      #{const SDL_SCANCODE_O} -> ScancodeO
      #{const SDL_SCANCODE_P} -> ScancodeP
      #{const SDL_SCANCODE_Q} -> ScancodeQ
      #{const SDL_SCANCODE_R} -> ScancodeR
      #{const SDL_SCANCODE_S} -> ScancodeS
      #{const SDL_SCANCODE_T} -> ScancodeT
      #{const SDL_SCANCODE_U} -> ScancodeU
      #{const SDL_SCANCODE_V} -> ScancodeV
      #{const SDL_SCANCODE_W} -> ScancodeW
      #{const SDL_SCANCODE_X} -> ScancodeX
      #{const SDL_SCANCODE_Y} -> ScancodeY
      #{const SDL_SCANCODE_Z} -> ScancodeZ
      #{const SDL_SCANCODE_1} -> Scancode1
      #{const SDL_SCANCODE_2} -> Scancode2
      #{const SDL_SCANCODE_3} -> Scancode3
      #{const SDL_SCANCODE_4} -> Scancode4
      #{const SDL_SCANCODE_5} -> Scancode5
      #{const SDL_SCANCODE_6} -> Scancode6
      #{const SDL_SCANCODE_7} -> Scancode7
      #{const SDL_SCANCODE_8} -> Scancode8
      #{const SDL_SCANCODE_9} -> Scancode9
      #{const SDL_SCANCODE_0} -> Scancode0
      #{const SDL_SCANCODE_RETURN} -> ScancodeReturn
      #{const SDL_SCANCODE_ESCAPE} -> ScancodeEscape
      #{const SDL_SCANCODE_BACKSPACE} -> ScancodeBackspace
      #{const SDL_SCANCODE_TAB} -> ScancodeTab
      #{const SDL_SCANCODE_SPACE} -> ScancodeSpace
      #{const SDL_SCANCODE_MINUS} -> ScancodeMinus
      #{const SDL_SCANCODE_EQUALS} -> ScancodeEquals
      #{const SDL_SCANCODE_LEFTBRACKET} -> ScancodeLeftbracket
      #{const SDL_SCANCODE_RIGHTBRACKET} -> ScancodeRightbracket
      #{const SDL_SCANCODE_BACKSLASH} -> ScancodeBackslash
      #{const SDL_SCANCODE_NONUSHASH} -> ScancodeNonushash
      #{const SDL_SCANCODE_SEMICOLON} -> ScancodeSemicolon
      #{const SDL_SCANCODE_APOSTROPHE} -> ScancodeApostrophe
      #{const SDL_SCANCODE_GRAVE} -> ScancodeGrave
      #{const SDL_SCANCODE_COMMA} -> ScancodeComma
      #{const SDL_SCANCODE_PERIOD} -> ScancodePeriod
      #{const SDL_SCANCODE_SLASH} -> ScancodeSlash
      #{const SDL_SCANCODE_CAPSLOCK} -> ScancodeCapslock
      #{const SDL_SCANCODE_F1} -> ScancodeF1
      #{const SDL_SCANCODE_F2} -> ScancodeF2
      #{const SDL_SCANCODE_F3} -> ScancodeF3
      #{const SDL_SCANCODE_F4} -> ScancodeF4
      #{const SDL_SCANCODE_F5} -> ScancodeF5
      #{const SDL_SCANCODE_F6} -> ScancodeF6
      #{const SDL_SCANCODE_F7} -> ScancodeF7
      #{const SDL_SCANCODE_F8} -> ScancodeF8
      #{const SDL_SCANCODE_F9} -> ScancodeF9
      #{const SDL_SCANCODE_F10} -> ScancodeF10
      #{const SDL_SCANCODE_F11} -> ScancodeF11
      #{const SDL_SCANCODE_F12} -> ScancodeF12
      #{const SDL_SCANCODE_PRINTSCREEN} -> ScancodePrintscreen
      #{const SDL_SCANCODE_SCROLLLOCK} -> ScancodeScrolllock
      #{const SDL_SCANCODE_PAUSE} -> ScancodePause
      #{const SDL_SCANCODE_INSERT} -> ScancodeInsert
      #{const SDL_SCANCODE_HOME} -> ScancodeHome
      #{const SDL_SCANCODE_PAGEUP} -> ScancodePageup
      #{const SDL_SCANCODE_DELETE} -> ScancodeDelete
      #{const SDL_SCANCODE_END} -> ScancodeEnd
      #{const SDL_SCANCODE_PAGEDOWN} -> ScancodePagedown
      #{const SDL_SCANCODE_RIGHT} -> ScancodeRight
      #{const SDL_SCANCODE_LEFT} -> ScancodeLeft
      #{const SDL_SCANCODE_DOWN} -> ScancodeDown
      #{const SDL_SCANCODE_UP} -> ScancodeUp
      #{const SDL_SCANCODE_NUMLOCKCLEAR} -> ScancodeNumlockclear
      #{const SDL_SCANCODE_KP_DIVIDE} -> ScancodeKPDivide
      #{const SDL_SCANCODE_KP_MULTIPLY} -> ScancodeKPMultiply
      #{const SDL_SCANCODE_KP_MINUS} -> ScancodeKPMinus
      #{const SDL_SCANCODE_KP_PLUS} -> ScancodeKPplus
      #{const SDL_SCANCODE_KP_ENTER} -> ScancodeKPEnter
      #{const SDL_SCANCODE_KP_1} -> ScancodeKP1
      #{const SDL_SCANCODE_KP_2} -> ScancodeKP2
      #{const SDL_SCANCODE_KP_3} -> ScancodeKP3
      #{const SDL_SCANCODE_KP_4} -> ScancodeKP4
      #{const SDL_SCANCODE_KP_5} -> ScancodeKP5
      #{const SDL_SCANCODE_KP_6} -> ScancodeKP6
      #{const SDL_SCANCODE_KP_7} -> ScancodeKP7
      #{const SDL_SCANCODE_KP_8} -> ScancodeKP8
      #{const SDL_SCANCODE_KP_9} -> ScancodeKP9
      #{const SDL_SCANCODE_KP_0} -> ScancodeKP0
      #{const SDL_SCANCODE_KP_PERIOD} -> ScancodeKPPeriod
      #{const SDL_SCANCODE_NONUSBACKSLASH} -> ScancodeNonusbackslash
      #{const SDL_SCANCODE_APPLICATION} -> ScancodeApplication
      #{const SDL_SCANCODE_POWER} -> ScancodePower
      #{const SDL_SCANCODE_KP_EQUALS} -> ScancodeKPEquals
      #{const SDL_SCANCODE_F13} -> ScancodeF13
      #{const SDL_SCANCODE_F14} -> ScancodeF14
      #{const SDL_SCANCODE_F15} -> ScancodeF15
      #{const SDL_SCANCODE_F16} -> ScancodeF16
      #{const SDL_SCANCODE_F17} -> ScancodeF17
      #{const SDL_SCANCODE_F18} -> ScancodeF18
      #{const SDL_SCANCODE_F19} -> ScancodeF19
      #{const SDL_SCANCODE_F20} -> ScancodeF20
      #{const SDL_SCANCODE_F21} -> ScancodeF21
      #{const SDL_SCANCODE_F22} -> ScancodeF22
      #{const SDL_SCANCODE_F23} -> ScancodeF23
      #{const SDL_SCANCODE_F24} -> ScancodeF24
      #{const SDL_SCANCODE_EXECUTE} -> ScancodeExecute
      #{const SDL_SCANCODE_HELP} -> ScancodeHelp
      #{const SDL_SCANCODE_MENU} -> ScancodeMenu
      #{const SDL_SCANCODE_SELECT} -> ScancodeSelect
      #{const SDL_SCANCODE_STOP} -> ScancodeStop
      #{const SDL_SCANCODE_AGAIN} -> ScancodeAgain
      #{const SDL_SCANCODE_UNDO} -> ScancodeUndo
      #{const SDL_SCANCODE_CUT} -> ScancodeCut
      #{const SDL_SCANCODE_COPY} -> ScancodeCopy
      #{const SDL_SCANCODE_PASTE} -> ScancodePaste
      #{const SDL_SCANCODE_FIND} -> ScancodeFind
      #{const SDL_SCANCODE_MUTE} -> ScancodeMute
      #{const SDL_SCANCODE_VOLUMEUP} -> ScancodeVolumeup
      #{const SDL_SCANCODE_VOLUMEDOWN} -> ScancodeVolumedown
      #{const SDL_SCANCODE_KP_COMMA} -> ScancodeKPComma
      #{const SDL_SCANCODE_KP_EQUALSAS400} -> ScancodeKPEqualsas400
      #{const SDL_SCANCODE_INTERNATIONAL1} -> ScancodeInternational1
      #{const SDL_SCANCODE_INTERNATIONAL2} -> ScancodeInternational2
      #{const SDL_SCANCODE_INTERNATIONAL3} -> ScancodeInternational3
      #{const SDL_SCANCODE_INTERNATIONAL4} -> ScancodeInternational4
      #{const SDL_SCANCODE_INTERNATIONAL5} -> ScancodeInternational5
      #{const SDL_SCANCODE_INTERNATIONAL6} -> ScancodeInternational6
      #{const SDL_SCANCODE_INTERNATIONAL7} -> ScancodeInternational7
      #{const SDL_SCANCODE_INTERNATIONAL8} -> ScancodeInternational8
      #{const SDL_SCANCODE_INTERNATIONAL9} -> ScancodeInternational9
      #{const SDL_SCANCODE_LANG1} -> ScancodeLang1
      #{const SDL_SCANCODE_LANG2} -> ScancodeLang2
      #{const SDL_SCANCODE_LANG3} -> ScancodeLang3
      #{const SDL_SCANCODE_LANG4} -> ScancodeLang4
      #{const SDL_SCANCODE_LANG5} -> ScancodeLang5
      #{const SDL_SCANCODE_LANG6} -> ScancodeLang6
      #{const SDL_SCANCODE_LANG7} -> ScancodeLang7
      #{const SDL_SCANCODE_LANG8} -> ScancodeLang8
      #{const SDL_SCANCODE_LANG9} -> ScancodeLang9
      #{const SDL_SCANCODE_ALTERASE} -> ScancodeAlterase
      #{const SDL_SCANCODE_SYSREQ} -> ScancodeSysreq
      #{const SDL_SCANCODE_CANCEL} -> ScancodeCancel
      #{const SDL_SCANCODE_CLEAR} -> ScancodeClear
      #{const SDL_SCANCODE_PRIOR} -> ScancodePrior
      #{const SDL_SCANCODE_RETURN2} -> ScancodeReturn2
      #{const SDL_SCANCODE_SEPARATOR} -> ScancodeSeparator
      #{const SDL_SCANCODE_OUT} -> ScancodeOut
      #{const SDL_SCANCODE_OPER} -> ScancodeOper
      #{const SDL_SCANCODE_CLEARAGAIN} -> ScancodeClearagain
      #{const SDL_SCANCODE_CRSEL} -> ScancodeCrsel
      #{const SDL_SCANCODE_EXSEL} -> ScancodeExsel
      #{const SDL_SCANCODE_KP_00} -> ScancodeKP00
      #{const SDL_SCANCODE_KP_000} -> ScancodeKP000
      #{const SDL_SCANCODE_THOUSANDSSEPARATOR} -> ScancodeThousandsseparator
      #{const SDL_SCANCODE_DECIMALSEPARATOR} -> ScancodeDecimalseparator
      #{const SDL_SCANCODE_CURRENCYUNIT} -> ScancodeCurrencyunit
      #{const SDL_SCANCODE_CURRENCYSUBUNIT} -> ScancodeCurrencysubunit
      #{const SDL_SCANCODE_KP_LEFTPAREN} -> ScancodeKPLeftparen
      #{const SDL_SCANCODE_KP_RIGHTPAREN} -> ScancodeKPRightparen
      #{const SDL_SCANCODE_KP_LEFTBRACE} -> ScancodeKPLeftbrace
      #{const SDL_SCANCODE_KP_RIGHTBRACE} -> ScancodeKPRightbrace
      #{const SDL_SCANCODE_KP_TAB} -> ScancodeKPTab
      #{const SDL_SCANCODE_KP_BACKSPACE} -> ScancodeKPBackspace
      #{const SDL_SCANCODE_KP_A} -> ScancodeKPA
      #{const SDL_SCANCODE_KP_B} -> ScancodeKPB
      #{const SDL_SCANCODE_KP_C} -> ScancodeKPC
      #{const SDL_SCANCODE_KP_D} -> ScancodeKPD
      #{const SDL_SCANCODE_KP_E} -> ScancodeKPE
      #{const SDL_SCANCODE_KP_F} -> ScancodeKPF
      #{const SDL_SCANCODE_KP_XOR} -> ScancodeKPXor
      #{const SDL_SCANCODE_KP_POWER} -> ScancodeKPPower
      #{const SDL_SCANCODE_KP_PERCENT} -> ScancodeKPPercent
      #{const SDL_SCANCODE_KP_LESS} -> ScancodeKPLess
      #{const SDL_SCANCODE_KP_GREATER} -> ScancodeKPGreater
      #{const SDL_SCANCODE_KP_AMPERSAND} -> ScancodeKPAmpersand
      #{const SDL_SCANCODE_KP_DBLAMPERSAND} -> ScancodeKPDblampersand
      #{const SDL_SCANCODE_KP_VERTICALBAR} -> ScancodeKPVerticalbar
      #{const SDL_SCANCODE_KP_DBLVERTICALBAR} -> ScancodeKPDblverticalbar
      #{const SDL_SCANCODE_KP_COLON} -> ScancodeKPColon
      #{const SDL_SCANCODE_KP_HASH} -> ScancodeKPHash
      #{const SDL_SCANCODE_KP_SPACE} -> ScancodeKPSpace
      #{const SDL_SCANCODE_KP_AT} -> ScancodeKPAt
      #{const SDL_SCANCODE_KP_EXCLAM} -> ScancodeKPExclam
      #{const SDL_SCANCODE_KP_MEMSTORE} -> ScancodeKPMemstore
      #{const SDL_SCANCODE_KP_MEMRECALL} -> ScancodeKPMemrecall
      #{const SDL_SCANCODE_KP_MEMCLEAR} -> ScancodeKPMemclear
      #{const SDL_SCANCODE_KP_MEMADD} -> ScancodeKPMemadd
      #{const SDL_SCANCODE_KP_MEMSUBTRACT} -> ScancodeKPMemsubtract
      #{const SDL_SCANCODE_KP_MEMMULTIPLY} -> ScancodeKPMemmultiply
      #{const SDL_SCANCODE_KP_MEMDIVIDE} -> ScancodeKPMemdivide
      #{const SDL_SCANCODE_KP_PLUSMINUS} -> ScancodeKPPlusminus
      #{const SDL_SCANCODE_KP_CLEAR} -> ScancodeKPClear
      #{const SDL_SCANCODE_KP_CLEARENTRY} -> ScancodeKPClearentry
      #{const SDL_SCANCODE_KP_BINARY} -> ScancodeKPBinary
      #{const SDL_SCANCODE_KP_OCTAL} -> ScancodeKPOctal
      #{const SDL_SCANCODE_KP_DECIMAL} -> ScancodeKPDecimal
      #{const SDL_SCANCODE_KP_HEXADECIMAL} -> ScancodeKPHexadecimal
      #{const SDL_SCANCODE_LCTRL} -> ScancodeLCtrl
      #{const SDL_SCANCODE_LSHIFT} -> ScancodeLShift
      #{const SDL_SCANCODE_LALT} -> ScancodeLAlt
      #{const SDL_SCANCODE_LGUI} -> ScancodeLGUI
      #{const SDL_SCANCODE_RCTRL} -> ScancodeRCtrl
      #{const SDL_SCANCODE_RSHIFT} -> ScancodeRShift
      #{const SDL_SCANCODE_RALT} -> ScancodeRAlt
      #{const SDL_SCANCODE_RGUI} -> ScancodeRGUI
      #{const SDL_SCANCODE_MODE} -> ScancodeMode
      #{const SDL_SCANCODE_AUDIONEXT} -> ScancodeAudionext
      #{const SDL_SCANCODE_AUDIOPREV} -> ScancodeAudioprev
      #{const SDL_SCANCODE_AUDIOSTOP} -> ScancodeAudiostop
      #{const SDL_SCANCODE_AUDIOPLAY} -> ScancodeAudioplay
      #{const SDL_SCANCODE_AUDIOMUTE} -> ScancodeAudiomute
      #{const SDL_SCANCODE_MEDIASELECT} -> ScancodeMediaselect
      #{const SDL_SCANCODE_WWW} -> ScancodeWWW
      #{const SDL_SCANCODE_MAIL} -> ScancodeMail
      #{const SDL_SCANCODE_CALCULATOR} -> ScancodeCalculator
      #{const SDL_SCANCODE_COMPUTER} -> ScancodeComputer
      #{const SDL_SCANCODE_AC_SEARCH} -> ScancodeACSearch
      #{const SDL_SCANCODE_AC_HOME} -> ScancodeACHome
      #{const SDL_SCANCODE_AC_BACK} -> ScancodeACBack
      #{const SDL_SCANCODE_AC_FORWARD} -> ScancodeACForward
      #{const SDL_SCANCODE_AC_STOP} -> ScancodeACStop
      #{const SDL_SCANCODE_AC_REFRESH} -> ScancodeACRefresh
      #{const SDL_SCANCODE_AC_BOOKMARKS} -> ScancodeACBookmarks
      #{const SDL_SCANCODE_BRIGHTNESSDOWN} -> ScancodeBrightnessDown
      #{const SDL_SCANCODE_BRIGHTNESSUP} -> ScancodeBrightnessUp
      #{const SDL_SCANCODE_DISPLAYSWITCH} -> ScancodeDisplaySwitch
      #{const SDL_SCANCODE_KBDILLUMTOGGLE} -> ScancodeKBDILLUMTOGGLE
      #{const SDL_SCANCODE_KBDILLUMDOWN} -> ScancodeKBDIlluMDown
      #{const SDL_SCANCODE_KBDILLUMUP} -> ScancodeKBDIllumUp
      #{const SDL_SCANCODE_EJECT} -> ScancodeEject
      #{const SDL_SCANCODE_SLEEP} -> ScancodeSleep
      #{const SDL_SCANCODE_APP1} -> ScancodeApp1
      #{const SDL_SCANCODE_APP2} -> ScancodeApp2
      _ -> error "toScancode: unhandled scancode"

data Keymod
   = KeymodNone
   | KeymodLShift
   | KeymodRShift
   | KeymodLCtrl
   | KeymodRCtrl
   | KeymodLAlt
   | KeymodRAlt
   | KeymodLGui
   | KeymodRGui
   | KeymodNum
   | KeymodCaps
   | KeymodMode
   | KeymodReserved
   deriving (Eq, Show)

fromKeymod :: Keymod -> #{type SDL_Keymod}
fromKeymod KeymodNone = #{const KMOD_NONE}
fromKeymod KeymodLShift = #{const KMOD_LSHIFT}
fromKeymod KeymodRShift = #{const KMOD_RSHIFT}
fromKeymod KeymodLCtrl = #{const KMOD_LCTRL}
fromKeymod KeymodRCtrl = #{const KMOD_RCTRL}
fromKeymod KeymodLAlt = #{const KMOD_LALT}
fromKeymod KeymodRAlt = #{const KMOD_RALT}
fromKeymod KeymodLGui = #{const KMOD_LGUI}
fromKeymod KeymodRGui = #{const KMOD_RGUI}
fromKeymod KeymodNum = #{const KMOD_NUM}
fromKeymod KeymodCaps = #{const KMOD_CAPS}
fromKeymod KeymodMode = #{const KMOD_MODE}
fromKeymod KeymodReserved = #{const KMOD_RESERVED}

toKeymod :: #{type SDL_Keymod} -> Keymod
toKeymod #{const KMOD_NONE} = KeymodNone
toKeymod #{const KMOD_LSHIFT} = KeymodLShift
toKeymod #{const KMOD_RSHIFT} = KeymodRShift
toKeymod #{const KMOD_LCTRL} = KeymodLCtrl
toKeymod #{const KMOD_RCTRL} = KeymodRCtrl
toKeymod #{const KMOD_LALT} = KeymodLAlt
toKeymod #{const KMOD_RALT} = KeymodRAlt
toKeymod #{const KMOD_LGUI} = KeymodLGui
toKeymod #{const KMOD_RGUI} = KeymodRGui
toKeymod #{const KMOD_NUM} = KeymodNum
toKeymod #{const KMOD_CAPS} = KeymodCaps
toKeymod #{const KMOD_MODE} = KeymodMode
toKeymod #{const KMOD_RESERVED} = KeymodReserved
toKeymod _ = error "unhandled keymod"

foreign import ccall unsafe "SDL_GetKeyboardFocus"
  sdlGetKeyboardFocus :: IO (Ptr WindowStruct)

getKeyboardFocus :: IO Window
getKeyboardFocus =
  sdlGetKeyboardFocus >>= mkFinalizedWindow

foreign import ccall unsafe "SDL_GetModState"
  sdlGetModState :: IO #{type SDL_Keymod}

getModState :: IO Keymod
getModState = sdlGetModState >>= return . toKeymod

foreign import ccall unsafe "SDL_SetModState"
  sdlSetModState :: #{type SDL_Keymod} -> IO ()

setModState :: Keymod -> IO ()
setModState keymod = sdlSetModState $ fromKeymod keymod

foreign import ccall unsafe "SDL_GetKeyFromScancode"
  sdlGetKeyFromScancode :: #{type SDL_Scancode} -> IO #{type SDL_Keycode}

getKeyFromScancode :: Scancode -> IO Keycode
getKeyFromScancode sc =
  let sc' = fromIntegral $ fromEnum sc
  in toEnum . fromIntegral <$> sdlGetKeyFromScancode sc'

foreign import ccall unsafe "SDL_GetScancodeFromKey"
  sdlGetScancodeFromKey :: #{type SDL_Keycode} -> IO #{type SDL_Scancode}

getScancodeFromKey :: Keycode -> IO Scancode
getScancodeFromKey kc =
  let kc' = fromIntegral $ fromEnum kc
  in toEnum . fromIntegral <$> sdlGetScancodeFromKey kc'

foreign import ccall unsafe "SDL_GetScancodeName"
  sdlGetScancodeName :: #{type SDL_Scancode} -> IO CString

getScancodeName :: Scancode -> IO String
getScancodeName sc =
  let sc' = fromIntegral $ fromEnum sc
  in sdlGetScancodeName sc' >>= peekCString

foreign import ccall unsafe "SDL_GetScancodeFromName"
  sdlGetScancodeFromName :: CString -> IO #{type SDL_Scancode}

getScancodeFromName :: String -> IO Scancode
getScancodeFromName name =
  withCString name $ \name' ->
    toEnum . fromIntegral <$> sdlGetScancodeFromName name'

foreign import ccall unsafe "SDL_GetKeyName"
  sdlGetKeyName :: #{type SDL_Keycode} -> IO CString

getKeyName :: Keycode -> IO String
getKeyName kc =
  let kc' = fromIntegral $ fromEnum kc
  in sdlGetKeyName kc' >>= peekCString

foreign import ccall unsafe "SDL_GetKeyFromName"
  sdlGetKeyFromName :: CString -> IO #{type SDL_Keycode}

getKeyFromName :: String -> IO Keycode
getKeyFromName name =
  withCString name $ \name' ->
    toEnum . fromIntegral <$> sdlGetKeyFromName name'

foreign import ccall unsafe "SDL_StartTextInput"
  startTextInput :: IO ()

foreign import ccall unsafe "SDL_IsTextInputActive"
  sdlIsTextInputActive :: IO #{type SDL_bool}

isTextInputActive :: IO Bool
isTextInputActive = sdlBoolToBool <$> sdlIsTextInputActive

foreign import ccall unsafe "SDL_StopTextInput"
  stopTextInput :: IO ()

foreign import ccall unsafe "SDL_SetTextInputRect"
  sdlSetTextInputRect :: Ptr Rect -> IO ()

setTextInputRect :: Rect -> IO ()
setTextInputRect = flip with sdlSetTextInputRect

foreign import ccall unsafe "SDL_HasScreenKeyboardSupport"
  sdlHasScreenKeyboardSupport :: IO #{type SDL_bool}

hasScreenKeyboardSupport :: IO Bool
hasScreenKeyboardSupport = sdlBoolToBool <$> sdlHasScreenKeyboardSupport

foreign import ccall unsafe "SDL_IsScreenKeyboardShown"
  sdlIsScreenKeyboardShown :: Ptr WindowStruct -> IO #{type SDL_bool}

isScreenKeyboardShown :: Window -> IO Bool
isScreenKeyboardShown window =
  withForeignPtr window $ \window' ->
    sdlBoolToBool <$> sdlIsScreenKeyboardShown window'

