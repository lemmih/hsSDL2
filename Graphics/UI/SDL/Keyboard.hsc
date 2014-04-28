#include "SDL.h"
module Graphics.UI.SDL.Keyboard
  ( Keymod(..)
  , Scancode(..)
  , getKeyboardFocus
  , getModState
  , setModState
  ) where

import Foreign
import Graphics.UI.SDL.Types
import Graphics.UI.SDL.Raw

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

fromScancode :: Scancode -> #{type int}
fromScancode ScancodeUNKNOWN = #{const SDL_SCANCODE_UNKNOWN}
fromScancode ScancodeA = #{const SDL_SCANCODE_A}
fromScancode ScancodeB = #{const SDL_SCANCODE_B}
fromScancode ScancodeC = #{const SDL_SCANCODE_C}
fromScancode ScancodeD = #{const SDL_SCANCODE_D}
fromScancode ScancodeE = #{const SDL_SCANCODE_E}
fromScancode ScancodeF = #{const SDL_SCANCODE_F}
fromScancode ScancodeG = #{const SDL_SCANCODE_G}
fromScancode ScancodeH = #{const SDL_SCANCODE_H}
fromScancode ScancodeI = #{const SDL_SCANCODE_I}
fromScancode ScancodeJ = #{const SDL_SCANCODE_J}
fromScancode ScancodeK = #{const SDL_SCANCODE_K}
fromScancode ScancodeL = #{const SDL_SCANCODE_L}
fromScancode ScancodeM = #{const SDL_SCANCODE_M}
fromScancode ScancodeN = #{const SDL_SCANCODE_N}
fromScancode ScancodeO = #{const SDL_SCANCODE_O}
fromScancode ScancodeP = #{const SDL_SCANCODE_P}
fromScancode ScancodeQ = #{const SDL_SCANCODE_Q}
fromScancode ScancodeR = #{const SDL_SCANCODE_R}
fromScancode ScancodeS = #{const SDL_SCANCODE_S}
fromScancode ScancodeT = #{const SDL_SCANCODE_T}
fromScancode ScancodeU = #{const SDL_SCANCODE_U}
fromScancode ScancodeV = #{const SDL_SCANCODE_V}
fromScancode ScancodeW = #{const SDL_SCANCODE_W}
fromScancode ScancodeX = #{const SDL_SCANCODE_X}
fromScancode ScancodeY = #{const SDL_SCANCODE_Y}
fromScancode ScancodeZ = #{const SDL_SCANCODE_Z}
fromScancode Scancode1 = #{const SDL_SCANCODE_1}
fromScancode Scancode2 = #{const SDL_SCANCODE_2}
fromScancode Scancode3 = #{const SDL_SCANCODE_3}
fromScancode Scancode4 = #{const SDL_SCANCODE_4}
fromScancode Scancode5 = #{const SDL_SCANCODE_5}
fromScancode Scancode6 = #{const SDL_SCANCODE_6}
fromScancode Scancode7 = #{const SDL_SCANCODE_7}
fromScancode Scancode8 = #{const SDL_SCANCODE_8}
fromScancode Scancode9 = #{const SDL_SCANCODE_9}
fromScancode Scancode0 = #{const SDL_SCANCODE_0}
fromScancode ScancodeReturn = #{const SDL_SCANCODE_RETURN}
fromScancode ScancodeEscape = #{const SDL_SCANCODE_ESCAPE}
fromScancode ScancodeBackspace = #{const SDL_SCANCODE_BACKSPACE}
fromScancode ScancodeTab = #{const SDL_SCANCODE_TAB}
fromScancode ScancodeSpace = #{const SDL_SCANCODE_SPACE}
fromScancode ScancodeMinus = #{const SDL_SCANCODE_MINUS}
fromScancode ScancodeEquals = #{const SDL_SCANCODE_EQUALS}
fromScancode ScancodeLeftbracket = #{const SDL_SCANCODE_LEFTBRACKET}
fromScancode ScancodeRightbracket = #{const SDL_SCANCODE_RIGHTBRACKET}
fromScancode ScancodeBackslash = #{const SDL_SCANCODE_BACKSLASH}
fromScancode ScancodeNonushash = #{const SDL_SCANCODE_NONUSHASH}
fromScancode ScancodeSemicolon = #{const SDL_SCANCODE_SEMICOLON}
fromScancode ScancodeApostrophe = #{const SDL_SCANCODE_APOSTROPHE}
fromScancode ScancodeGrave = #{const SDL_SCANCODE_GRAVE}
fromScancode ScancodeComma = #{const SDL_SCANCODE_COMMA}
fromScancode ScancodePeriod = #{const SDL_SCANCODE_PERIOD}
fromScancode ScancodeSlash = #{const SDL_SCANCODE_SLASH}
fromScancode ScancodeCapslock = #{const SDL_SCANCODE_CAPSLOCK}
fromScancode ScancodeF1 = #{const SDL_SCANCODE_F1}
fromScancode ScancodeF2 = #{const SDL_SCANCODE_F2}
fromScancode ScancodeF3 = #{const SDL_SCANCODE_F3}
fromScancode ScancodeF4 = #{const SDL_SCANCODE_F4}
fromScancode ScancodeF5 = #{const SDL_SCANCODE_F5}
fromScancode ScancodeF6 = #{const SDL_SCANCODE_F6}
fromScancode ScancodeF7 = #{const SDL_SCANCODE_F7}
fromScancode ScancodeF8 = #{const SDL_SCANCODE_F8}
fromScancode ScancodeF9 = #{const SDL_SCANCODE_F9}
fromScancode ScancodeF10 = #{const SDL_SCANCODE_F10}
fromScancode ScancodeF11 = #{const SDL_SCANCODE_F11}
fromScancode ScancodeF12 = #{const SDL_SCANCODE_F12}
fromScancode ScancodePrintscreen = #{const SDL_SCANCODE_PRINTSCREEN}
fromScancode ScancodeScrolllock = #{const SDL_SCANCODE_SCROLLLOCK}
fromScancode ScancodePause = #{const SDL_SCANCODE_PAUSE}
fromScancode ScancodeInsert = #{const SDL_SCANCODE_INSERT}
fromScancode ScancodeHome = #{const SDL_SCANCODE_HOME}
fromScancode ScancodePageup = #{const SDL_SCANCODE_PAGEUP}
fromScancode ScancodeDelete = #{const SDL_SCANCODE_DELETE}
fromScancode ScancodeEnd = #{const SDL_SCANCODE_END}
fromScancode ScancodePagedown = #{const SDL_SCANCODE_PAGEDOWN}
fromScancode ScancodeRight = #{const SDL_SCANCODE_RIGHT}
fromScancode ScancodeLeft = #{const SDL_SCANCODE_LEFT}
fromScancode ScancodeDown = #{const SDL_SCANCODE_DOWN}
fromScancode ScancodeUp = #{const SDL_SCANCODE_UP}
fromScancode ScancodeNumlockclear = #{const SDL_SCANCODE_NUMLOCKCLEAR}
fromScancode ScancodeKPDivide = #{const SDL_SCANCODE_KP_DIVIDE}
fromScancode ScancodeKPMultiply = #{const SDL_SCANCODE_KP_MULTIPLY}
fromScancode ScancodeKPMinus = #{const SDL_SCANCODE_KP_MINUS}
fromScancode ScancodeKPplus = #{const SDL_SCANCODE_KP_PLUS}
fromScancode ScancodeKPEnter = #{const SDL_SCANCODE_KP_ENTER}
fromScancode ScancodeKP1 = #{const SDL_SCANCODE_KP_1}
fromScancode ScancodeKP2 = #{const SDL_SCANCODE_KP_2}
fromScancode ScancodeKP3 = #{const SDL_SCANCODE_KP_3}
fromScancode ScancodeKP4 = #{const SDL_SCANCODE_KP_4}
fromScancode ScancodeKP5 = #{const SDL_SCANCODE_KP_5}
fromScancode ScancodeKP6 = #{const SDL_SCANCODE_KP_6}
fromScancode ScancodeKP7 = #{const SDL_SCANCODE_KP_7}
fromScancode ScancodeKP8 = #{const SDL_SCANCODE_KP_8}
fromScancode ScancodeKP9 = #{const SDL_SCANCODE_KP_9}
fromScancode ScancodeKP0 = #{const SDL_SCANCODE_KP_0}
fromScancode ScancodeKPPeriod = #{const SDL_SCANCODE_KP_PERIOD}
fromScancode ScancodeNonusbackslash = #{const SDL_SCANCODE_NONUSBACKSLASH}
fromScancode ScancodeApplication = #{const SDL_SCANCODE_APPLICATION}
fromScancode ScancodePower = #{const SDL_SCANCODE_POWER}
fromScancode ScancodeKPEquals = #{const SDL_SCANCODE_KP_EQUALS}
fromScancode ScancodeF13 = #{const SDL_SCANCODE_F13}
fromScancode ScancodeF14 = #{const SDL_SCANCODE_F14}
fromScancode ScancodeF15 = #{const SDL_SCANCODE_F15}
fromScancode ScancodeF16 = #{const SDL_SCANCODE_F16}
fromScancode ScancodeF17 = #{const SDL_SCANCODE_F17}
fromScancode ScancodeF18 = #{const SDL_SCANCODE_F18}
fromScancode ScancodeF19 = #{const SDL_SCANCODE_F19}
fromScancode ScancodeF20 = #{const SDL_SCANCODE_F20}
fromScancode ScancodeF21 = #{const SDL_SCANCODE_F21}
fromScancode ScancodeF22 = #{const SDL_SCANCODE_F22}
fromScancode ScancodeF23 = #{const SDL_SCANCODE_F23}
fromScancode ScancodeF24 = #{const SDL_SCANCODE_F24}
fromScancode ScancodeExecute = #{const SDL_SCANCODE_EXECUTE}
fromScancode ScancodeHelp = #{const SDL_SCANCODE_HELP}
fromScancode ScancodeMenu = #{const SDL_SCANCODE_MENU}
fromScancode ScancodeSelect = #{const SDL_SCANCODE_SELECT}
fromScancode ScancodeStop = #{const SDL_SCANCODE_STOP}
fromScancode ScancodeAgain = #{const SDL_SCANCODE_AGAIN}
fromScancode ScancodeUndo = #{const SDL_SCANCODE_UNDO}
fromScancode ScancodeCut = #{const SDL_SCANCODE_CUT}
fromScancode ScancodeCopy = #{const SDL_SCANCODE_COPY}
fromScancode ScancodePaste = #{const SDL_SCANCODE_PASTE}
fromScancode ScancodeFind = #{const SDL_SCANCODE_FIND}
fromScancode ScancodeMute = #{const SDL_SCANCODE_MUTE}
fromScancode ScancodeVolumeup = #{const SDL_SCANCODE_VOLUMEUP}
fromScancode ScancodeVolumedown = #{const SDL_SCANCODE_VOLUMEDOWN}
fromScancode ScancodeKPComma = #{const SDL_SCANCODE_KP_COMMA}
fromScancode ScancodeKPEqualsas400 = #{const SDL_SCANCODE_KP_EQUALSAS400}
fromScancode ScancodeInternational1 = #{const SDL_SCANCODE_INTERNATIONAL1}
fromScancode ScancodeInternational2 = #{const SDL_SCANCODE_INTERNATIONAL2}
fromScancode ScancodeInternational3 = #{const SDL_SCANCODE_INTERNATIONAL3}
fromScancode ScancodeInternational4 = #{const SDL_SCANCODE_INTERNATIONAL4}
fromScancode ScancodeInternational5 = #{const SDL_SCANCODE_INTERNATIONAL5}
fromScancode ScancodeInternational6 = #{const SDL_SCANCODE_INTERNATIONAL6}
fromScancode ScancodeInternational7 = #{const SDL_SCANCODE_INTERNATIONAL7}
fromScancode ScancodeInternational8 = #{const SDL_SCANCODE_INTERNATIONAL8}
fromScancode ScancodeInternational9 = #{const SDL_SCANCODE_INTERNATIONAL9}
fromScancode ScancodeLang1 = #{const SDL_SCANCODE_LANG1}
fromScancode ScancodeLang2 = #{const SDL_SCANCODE_LANG2}
fromScancode ScancodeLang3 = #{const SDL_SCANCODE_LANG3}
fromScancode ScancodeLang4 = #{const SDL_SCANCODE_LANG4}
fromScancode ScancodeLang5 = #{const SDL_SCANCODE_LANG5}
fromScancode ScancodeLang6 = #{const SDL_SCANCODE_LANG6}
fromScancode ScancodeLang7 = #{const SDL_SCANCODE_LANG7}
fromScancode ScancodeLang8 = #{const SDL_SCANCODE_LANG8}
fromScancode ScancodeLang9 = #{const SDL_SCANCODE_LANG9}
fromScancode ScancodeAlterase = #{const SDL_SCANCODE_ALTERASE}
fromScancode ScancodeSysreq = #{const SDL_SCANCODE_SYSREQ}
fromScancode ScancodeCancel = #{const SDL_SCANCODE_CANCEL}
fromScancode ScancodeClear = #{const SDL_SCANCODE_CLEAR}
fromScancode ScancodePrior = #{const SDL_SCANCODE_PRIOR}
fromScancode ScancodeReturn2 = #{const SDL_SCANCODE_RETURN2}
fromScancode ScancodeSeparator = #{const SDL_SCANCODE_SEPARATOR}
fromScancode ScancodeOut = #{const SDL_SCANCODE_OUT}
fromScancode ScancodeOper = #{const SDL_SCANCODE_OPER}
fromScancode ScancodeClearagain = #{const SDL_SCANCODE_CLEARAGAIN}
fromScancode ScancodeCrsel = #{const SDL_SCANCODE_CRSEL}
fromScancode ScancodeExsel = #{const SDL_SCANCODE_EXSEL}
fromScancode ScancodeKP00 = #{const SDL_SCANCODE_KP_00}
fromScancode ScancodeKP000 = #{const SDL_SCANCODE_KP_000}
fromScancode ScancodeThousandsseparator = #{const SDL_SCANCODE_THOUSANDSSEPARATOR}
fromScancode ScancodeDecimalseparator = #{const SDL_SCANCODE_DECIMALSEPARATOR}
fromScancode ScancodeCurrencyunit = #{const SDL_SCANCODE_CURRENCYUNIT}
fromScancode ScancodeCurrencysubunit = #{const SDL_SCANCODE_CURRENCYSUBUNIT}
fromScancode ScancodeKPLeftparen = #{const SDL_SCANCODE_KP_LEFTPAREN}
fromScancode ScancodeKPRightparen = #{const SDL_SCANCODE_KP_RIGHTPAREN}
fromScancode ScancodeKPLeftbrace = #{const SDL_SCANCODE_KP_LEFTBRACE}
fromScancode ScancodeKPRightbrace = #{const SDL_SCANCODE_KP_RIGHTBRACE}
fromScancode ScancodeKPTab = #{const SDL_SCANCODE_KP_TAB}
fromScancode ScancodeKPBackspace = #{const SDL_SCANCODE_KP_BACKSPACE}
fromScancode ScancodeKPA = #{const SDL_SCANCODE_KP_A}
fromScancode ScancodeKPB = #{const SDL_SCANCODE_KP_B}
fromScancode ScancodeKPC = #{const SDL_SCANCODE_KP_C}
fromScancode ScancodeKPD = #{const SDL_SCANCODE_KP_D}
fromScancode ScancodeKPE = #{const SDL_SCANCODE_KP_E}
fromScancode ScancodeKPF = #{const SDL_SCANCODE_KP_F}
fromScancode ScancodeKPXor = #{const SDL_SCANCODE_KP_XOR}
fromScancode ScancodeKPPower = #{const SDL_SCANCODE_KP_POWER}
fromScancode ScancodeKPPercent = #{const SDL_SCANCODE_KP_PERCENT}
fromScancode ScancodeKPLess = #{const SDL_SCANCODE_KP_LESS}
fromScancode ScancodeKPGreater = #{const SDL_SCANCODE_KP_GREATER}
fromScancode ScancodeKPAmpersand = #{const SDL_SCANCODE_KP_AMPERSAND}
fromScancode ScancodeKPDblampersand = #{const SDL_SCANCODE_KP_DBLAMPERSAND}
fromScancode ScancodeKPVerticalbar = #{const SDL_SCANCODE_KP_VERTICALBAR}
fromScancode ScancodeKPDblverticalbar = #{const SDL_SCANCODE_KP_DBLVERTICALBAR}
fromScancode ScancodeKPColon = #{const SDL_SCANCODE_KP_COLON}
fromScancode ScancodeKPHash = #{const SDL_SCANCODE_KP_HASH}
fromScancode ScancodeKPSpace = #{const SDL_SCANCODE_KP_SPACE}
fromScancode ScancodeKPAt = #{const SDL_SCANCODE_KP_AT}
fromScancode ScancodeKPExclam = #{const SDL_SCANCODE_KP_EXCLAM}
fromScancode ScancodeKPMemstore = #{const SDL_SCANCODE_KP_MEMSTORE}
fromScancode ScancodeKPMemrecall = #{const SDL_SCANCODE_KP_MEMRECALL}
fromScancode ScancodeKPMemclear = #{const SDL_SCANCODE_KP_MEMCLEAR}
fromScancode ScancodeKPMemadd = #{const SDL_SCANCODE_KP_MEMADD}
fromScancode ScancodeKPMemsubtract = #{const SDL_SCANCODE_KP_MEMSUBTRACT}
fromScancode ScancodeKPMemmultiply = #{const SDL_SCANCODE_KP_MEMMULTIPLY}
fromScancode ScancodeKPMemdivide = #{const SDL_SCANCODE_KP_MEMDIVIDE}
fromScancode ScancodeKPPlusminus = #{const SDL_SCANCODE_KP_PLUSMINUS}
fromScancode ScancodeKPClear = #{const SDL_SCANCODE_KP_CLEAR}
fromScancode ScancodeKPClearentry = #{const SDL_SCANCODE_KP_CLEARENTRY}
fromScancode ScancodeKPBinary = #{const SDL_SCANCODE_KP_BINARY}
fromScancode ScancodeKPOctal = #{const SDL_SCANCODE_KP_OCTAL}
fromScancode ScancodeKPDecimal = #{const SDL_SCANCODE_KP_DECIMAL}
fromScancode ScancodeKPHexadecimal = #{const SDL_SCANCODE_KP_HEXADECIMAL}
fromScancode ScancodeLCtrl = #{const SDL_SCANCODE_LCTRL}
fromScancode ScancodeLShift = #{const SDL_SCANCODE_LSHIFT}
fromScancode ScancodeLAlt = #{const SDL_SCANCODE_LALT}
fromScancode ScancodeLGUI = #{const SDL_SCANCODE_LGUI}
fromScancode ScancodeRCtrl = #{const SDL_SCANCODE_RCTRL}
fromScancode ScancodeRShift = #{const SDL_SCANCODE_RSHIFT}
fromScancode ScancodeRAlt = #{const SDL_SCANCODE_RALT}
fromScancode ScancodeRGUI = #{const SDL_SCANCODE_RGUI}
fromScancode ScancodeMode = #{const SDL_SCANCODE_MODE}
fromScancode ScancodeAudionext = #{const SDL_SCANCODE_AUDIONEXT}
fromScancode ScancodeAudioprev = #{const SDL_SCANCODE_AUDIOPREV}
fromScancode ScancodeAudiostop = #{const SDL_SCANCODE_AUDIOSTOP}
fromScancode ScancodeAudioplay = #{const SDL_SCANCODE_AUDIOPLAY}
fromScancode ScancodeAudiomute = #{const SDL_SCANCODE_AUDIOMUTE}
fromScancode ScancodeMediaselect = #{const SDL_SCANCODE_MEDIASELECT}
fromScancode ScancodeWWW = #{const SDL_SCANCODE_WWW}
fromScancode ScancodeMail = #{const SDL_SCANCODE_MAIL}
fromScancode ScancodeCalculator = #{const SDL_SCANCODE_CALCULATOR}
fromScancode ScancodeComputer = #{const SDL_SCANCODE_COMPUTER}
fromScancode ScancodeACSearch = #{const SDL_SCANCODE_AC_SEARCH}
fromScancode ScancodeACHome = #{const SDL_SCANCODE_AC_HOME}
fromScancode ScancodeACBack = #{const SDL_SCANCODE_AC_BACK}
fromScancode ScancodeACForward = #{const SDL_SCANCODE_AC_FORWARD}
fromScancode ScancodeACStop = #{const SDL_SCANCODE_AC_STOP}
fromScancode ScancodeACRefresh = #{const SDL_SCANCODE_AC_REFRESH}
fromScancode ScancodeACBookmarks = #{const SDL_SCANCODE_AC_BOOKMARKS}
fromScancode ScancodeBrightnessDown = #{const SDL_SCANCODE_BRIGHTNESSDOWN}
fromScancode ScancodeBrightnessUp = #{const SDL_SCANCODE_BRIGHTNESSUP}
fromScancode ScancodeDisplaySwitch = #{const SDL_SCANCODE_DISPLAYSWITCH}
fromScancode ScancodeKBDILLUMTOGGLE = #{const SDL_SCANCODE_KBDILLUMTOGGLE}
fromScancode ScancodeKBDIlluMDown = #{const SDL_SCANCODE_KBDILLUMDOWN}
fromScancode ScancodeKBDIllumUp = #{const SDL_SCANCODE_KBDILLUMUP}
fromScancode ScancodeEject = #{const SDL_SCANCODE_EJECT}
fromScancode ScancodeSleep = #{const SDL_SCANCODE_SLEEP}
fromScancode ScancodeApp1 = #{const SDL_SCANCODE_APP1}
fromScancode ScancodeApp2 = #{const SDL_SCANCODE_APP2}

toScancode :: #{type int} -> Scancode
toScancode #{const SDL_SCANCODE_UNKNOWN} = ScancodeUNKNOWN
toScancode #{const SDL_SCANCODE_A} = ScancodeA
toScancode #{const SDL_SCANCODE_B} = ScancodeB
toScancode #{const SDL_SCANCODE_C} = ScancodeC
toScancode #{const SDL_SCANCODE_D} = ScancodeD
toScancode #{const SDL_SCANCODE_E} = ScancodeE
toScancode #{const SDL_SCANCODE_F} = ScancodeF
toScancode #{const SDL_SCANCODE_G} = ScancodeG
toScancode #{const SDL_SCANCODE_H} = ScancodeH
toScancode #{const SDL_SCANCODE_I} = ScancodeI
toScancode #{const SDL_SCANCODE_J} = ScancodeJ
toScancode #{const SDL_SCANCODE_K} = ScancodeK
toScancode #{const SDL_SCANCODE_L} = ScancodeL
toScancode #{const SDL_SCANCODE_M} = ScancodeM
toScancode #{const SDL_SCANCODE_N} = ScancodeN
toScancode #{const SDL_SCANCODE_O} = ScancodeO
toScancode #{const SDL_SCANCODE_P} = ScancodeP
toScancode #{const SDL_SCANCODE_Q} = ScancodeQ
toScancode #{const SDL_SCANCODE_R} = ScancodeR
toScancode #{const SDL_SCANCODE_S} = ScancodeS
toScancode #{const SDL_SCANCODE_T} = ScancodeT
toScancode #{const SDL_SCANCODE_U} = ScancodeU
toScancode #{const SDL_SCANCODE_V} = ScancodeV
toScancode #{const SDL_SCANCODE_W} = ScancodeW
toScancode #{const SDL_SCANCODE_X} = ScancodeX
toScancode #{const SDL_SCANCODE_Y} = ScancodeY
toScancode #{const SDL_SCANCODE_Z} = ScancodeZ
toScancode #{const SDL_SCANCODE_1} = Scancode1
toScancode #{const SDL_SCANCODE_2} = Scancode2
toScancode #{const SDL_SCANCODE_3} = Scancode3
toScancode #{const SDL_SCANCODE_4} = Scancode4
toScancode #{const SDL_SCANCODE_5} = Scancode5
toScancode #{const SDL_SCANCODE_6} = Scancode6
toScancode #{const SDL_SCANCODE_7} = Scancode7
toScancode #{const SDL_SCANCODE_8} = Scancode8
toScancode #{const SDL_SCANCODE_9} = Scancode9
toScancode #{const SDL_SCANCODE_0} = Scancode0
toScancode #{const SDL_SCANCODE_RETURN} = ScancodeReturn
toScancode #{const SDL_SCANCODE_ESCAPE} = ScancodeEscape
toScancode #{const SDL_SCANCODE_BACKSPACE} = ScancodeBackspace
toScancode #{const SDL_SCANCODE_TAB} = ScancodeTab
toScancode #{const SDL_SCANCODE_SPACE} = ScancodeSpace
toScancode #{const SDL_SCANCODE_MINUS} = ScancodeMinus
toScancode #{const SDL_SCANCODE_EQUALS} = ScancodeEquals
toScancode #{const SDL_SCANCODE_LEFTBRACKET} = ScancodeLeftbracket
toScancode #{const SDL_SCANCODE_RIGHTBRACKET} = ScancodeRightbracket
toScancode #{const SDL_SCANCODE_BACKSLASH} = ScancodeBackslash
toScancode #{const SDL_SCANCODE_NONUSHASH} = ScancodeNonushash
toScancode #{const SDL_SCANCODE_SEMICOLON} = ScancodeSemicolon
toScancode #{const SDL_SCANCODE_APOSTROPHE} = ScancodeApostrophe
toScancode #{const SDL_SCANCODE_GRAVE} = ScancodeGrave
toScancode #{const SDL_SCANCODE_COMMA} = ScancodeComma
toScancode #{const SDL_SCANCODE_PERIOD} = ScancodePeriod
toScancode #{const SDL_SCANCODE_SLASH} = ScancodeSlash
toScancode #{const SDL_SCANCODE_CAPSLOCK} = ScancodeCapslock
toScancode #{const SDL_SCANCODE_F1} = ScancodeF1
toScancode #{const SDL_SCANCODE_F2} = ScancodeF2
toScancode #{const SDL_SCANCODE_F3} = ScancodeF3
toScancode #{const SDL_SCANCODE_F4} = ScancodeF4
toScancode #{const SDL_SCANCODE_F5} = ScancodeF5
toScancode #{const SDL_SCANCODE_F6} = ScancodeF6
toScancode #{const SDL_SCANCODE_F7} = ScancodeF7
toScancode #{const SDL_SCANCODE_F8} = ScancodeF8
toScancode #{const SDL_SCANCODE_F9} = ScancodeF9
toScancode #{const SDL_SCANCODE_F10} = ScancodeF10
toScancode #{const SDL_SCANCODE_F11} = ScancodeF11
toScancode #{const SDL_SCANCODE_F12} = ScancodeF12
toScancode #{const SDL_SCANCODE_PRINTSCREEN} = ScancodePrintscreen
toScancode #{const SDL_SCANCODE_SCROLLLOCK} = ScancodeScrolllock
toScancode #{const SDL_SCANCODE_PAUSE} = ScancodePause
toScancode #{const SDL_SCANCODE_INSERT} = ScancodeInsert
toScancode #{const SDL_SCANCODE_HOME} = ScancodeHome
toScancode #{const SDL_SCANCODE_PAGEUP} = ScancodePageup
toScancode #{const SDL_SCANCODE_DELETE} = ScancodeDelete
toScancode #{const SDL_SCANCODE_END} = ScancodeEnd
toScancode #{const SDL_SCANCODE_PAGEDOWN} = ScancodePagedown
toScancode #{const SDL_SCANCODE_RIGHT} = ScancodeRight
toScancode #{const SDL_SCANCODE_LEFT} = ScancodeLeft
toScancode #{const SDL_SCANCODE_DOWN} = ScancodeDown
toScancode #{const SDL_SCANCODE_UP} = ScancodeUp
toScancode #{const SDL_SCANCODE_NUMLOCKCLEAR} = ScancodeNumlockclear
toScancode #{const SDL_SCANCODE_KP_DIVIDE} = ScancodeKPDivide
toScancode #{const SDL_SCANCODE_KP_MULTIPLY} = ScancodeKPMultiply
toScancode #{const SDL_SCANCODE_KP_MINUS} = ScancodeKPMinus
toScancode #{const SDL_SCANCODE_KP_PLUS} = ScancodeKPplus
toScancode #{const SDL_SCANCODE_KP_ENTER} = ScancodeKPEnter
toScancode #{const SDL_SCANCODE_KP_1} = ScancodeKP1
toScancode #{const SDL_SCANCODE_KP_2} = ScancodeKP2
toScancode #{const SDL_SCANCODE_KP_3} = ScancodeKP3
toScancode #{const SDL_SCANCODE_KP_4} = ScancodeKP4
toScancode #{const SDL_SCANCODE_KP_5} = ScancodeKP5
toScancode #{const SDL_SCANCODE_KP_6} = ScancodeKP6
toScancode #{const SDL_SCANCODE_KP_7} = ScancodeKP7
toScancode #{const SDL_SCANCODE_KP_8} = ScancodeKP8
toScancode #{const SDL_SCANCODE_KP_9} = ScancodeKP9
toScancode #{const SDL_SCANCODE_KP_0} = ScancodeKP0
toScancode #{const SDL_SCANCODE_KP_PERIOD} = ScancodeKPPeriod
toScancode #{const SDL_SCANCODE_NONUSBACKSLASH} = ScancodeNonusbackslash
toScancode #{const SDL_SCANCODE_APPLICATION} = ScancodeApplication
toScancode #{const SDL_SCANCODE_POWER} = ScancodePower
toScancode #{const SDL_SCANCODE_KP_EQUALS} = ScancodeKPEquals
toScancode #{const SDL_SCANCODE_F13} = ScancodeF13
toScancode #{const SDL_SCANCODE_F14} = ScancodeF14
toScancode #{const SDL_SCANCODE_F15} = ScancodeF15
toScancode #{const SDL_SCANCODE_F16} = ScancodeF16
toScancode #{const SDL_SCANCODE_F17} = ScancodeF17
toScancode #{const SDL_SCANCODE_F18} = ScancodeF18
toScancode #{const SDL_SCANCODE_F19} = ScancodeF19
toScancode #{const SDL_SCANCODE_F20} = ScancodeF20
toScancode #{const SDL_SCANCODE_F21} = ScancodeF21
toScancode #{const SDL_SCANCODE_F22} = ScancodeF22
toScancode #{const SDL_SCANCODE_F23} = ScancodeF23
toScancode #{const SDL_SCANCODE_F24} = ScancodeF24
toScancode #{const SDL_SCANCODE_EXECUTE} = ScancodeExecute
toScancode #{const SDL_SCANCODE_HELP} = ScancodeHelp
toScancode #{const SDL_SCANCODE_MENU} = ScancodeMenu
toScancode #{const SDL_SCANCODE_SELECT} = ScancodeSelect
toScancode #{const SDL_SCANCODE_STOP} = ScancodeStop
toScancode #{const SDL_SCANCODE_AGAIN} = ScancodeAgain
toScancode #{const SDL_SCANCODE_UNDO} = ScancodeUndo
toScancode #{const SDL_SCANCODE_CUT} = ScancodeCut
toScancode #{const SDL_SCANCODE_COPY} = ScancodeCopy
toScancode #{const SDL_SCANCODE_PASTE} = ScancodePaste
toScancode #{const SDL_SCANCODE_FIND} = ScancodeFind
toScancode #{const SDL_SCANCODE_MUTE} = ScancodeMute
toScancode #{const SDL_SCANCODE_VOLUMEUP} = ScancodeVolumeup
toScancode #{const SDL_SCANCODE_VOLUMEDOWN} = ScancodeVolumedown
toScancode #{const SDL_SCANCODE_KP_COMMA} = ScancodeKPComma
toScancode #{const SDL_SCANCODE_KP_EQUALSAS400} = ScancodeKPEqualsas400
toScancode #{const SDL_SCANCODE_INTERNATIONAL1} = ScancodeInternational1
toScancode #{const SDL_SCANCODE_INTERNATIONAL2} = ScancodeInternational2
toScancode #{const SDL_SCANCODE_INTERNATIONAL3} = ScancodeInternational3
toScancode #{const SDL_SCANCODE_INTERNATIONAL4} = ScancodeInternational4
toScancode #{const SDL_SCANCODE_INTERNATIONAL5} = ScancodeInternational5
toScancode #{const SDL_SCANCODE_INTERNATIONAL6} = ScancodeInternational6
toScancode #{const SDL_SCANCODE_INTERNATIONAL7} = ScancodeInternational7
toScancode #{const SDL_SCANCODE_INTERNATIONAL8} = ScancodeInternational8
toScancode #{const SDL_SCANCODE_INTERNATIONAL9} = ScancodeInternational9
toScancode #{const SDL_SCANCODE_LANG1} = ScancodeLang1
toScancode #{const SDL_SCANCODE_LANG2} = ScancodeLang2
toScancode #{const SDL_SCANCODE_LANG3} = ScancodeLang3
toScancode #{const SDL_SCANCODE_LANG4} = ScancodeLang4
toScancode #{const SDL_SCANCODE_LANG5} = ScancodeLang5
toScancode #{const SDL_SCANCODE_LANG6} = ScancodeLang6
toScancode #{const SDL_SCANCODE_LANG7} = ScancodeLang7
toScancode #{const SDL_SCANCODE_LANG8} = ScancodeLang8
toScancode #{const SDL_SCANCODE_LANG9} = ScancodeLang9
toScancode #{const SDL_SCANCODE_ALTERASE} = ScancodeAlterase
toScancode #{const SDL_SCANCODE_SYSREQ} = ScancodeSysreq
toScancode #{const SDL_SCANCODE_CANCEL} = ScancodeCancel
toScancode #{const SDL_SCANCODE_CLEAR} = ScancodeClear
toScancode #{const SDL_SCANCODE_PRIOR} = ScancodePrior
toScancode #{const SDL_SCANCODE_RETURN2} = ScancodeReturn2
toScancode #{const SDL_SCANCODE_SEPARATOR} = ScancodeSeparator
toScancode #{const SDL_SCANCODE_OUT} = ScancodeOut
toScancode #{const SDL_SCANCODE_OPER} = ScancodeOper
toScancode #{const SDL_SCANCODE_CLEARAGAIN} = ScancodeClearagain
toScancode #{const SDL_SCANCODE_CRSEL} = ScancodeCrsel
toScancode #{const SDL_SCANCODE_EXSEL} = ScancodeExsel
toScancode #{const SDL_SCANCODE_KP_00} = ScancodeKP00
toScancode #{const SDL_SCANCODE_KP_000} = ScancodeKP000
toScancode #{const SDL_SCANCODE_THOUSANDSSEPARATOR} = ScancodeThousandsseparator
toScancode #{const SDL_SCANCODE_DECIMALSEPARATOR} = ScancodeDecimalseparator
toScancode #{const SDL_SCANCODE_CURRENCYUNIT} = ScancodeCurrencyunit
toScancode #{const SDL_SCANCODE_CURRENCYSUBUNIT} = ScancodeCurrencysubunit
toScancode #{const SDL_SCANCODE_KP_LEFTPAREN} = ScancodeKPLeftparen
toScancode #{const SDL_SCANCODE_KP_RIGHTPAREN} = ScancodeKPRightparen
toScancode #{const SDL_SCANCODE_KP_LEFTBRACE} = ScancodeKPLeftbrace
toScancode #{const SDL_SCANCODE_KP_RIGHTBRACE} = ScancodeKPRightbrace
toScancode #{const SDL_SCANCODE_KP_TAB} = ScancodeKPTab
toScancode #{const SDL_SCANCODE_KP_BACKSPACE} = ScancodeKPBackspace
toScancode #{const SDL_SCANCODE_KP_A} = ScancodeKPA
toScancode #{const SDL_SCANCODE_KP_B} = ScancodeKPB
toScancode #{const SDL_SCANCODE_KP_C} = ScancodeKPC
toScancode #{const SDL_SCANCODE_KP_D} = ScancodeKPD
toScancode #{const SDL_SCANCODE_KP_E} = ScancodeKPE
toScancode #{const SDL_SCANCODE_KP_F} = ScancodeKPF
toScancode #{const SDL_SCANCODE_KP_XOR} = ScancodeKPXor
toScancode #{const SDL_SCANCODE_KP_POWER} = ScancodeKPPower
toScancode #{const SDL_SCANCODE_KP_PERCENT} = ScancodeKPPercent
toScancode #{const SDL_SCANCODE_KP_LESS} = ScancodeKPLess
toScancode #{const SDL_SCANCODE_KP_GREATER} = ScancodeKPGreater
toScancode #{const SDL_SCANCODE_KP_AMPERSAND} = ScancodeKPAmpersand
toScancode #{const SDL_SCANCODE_KP_DBLAMPERSAND} = ScancodeKPDblampersand
toScancode #{const SDL_SCANCODE_KP_VERTICALBAR} = ScancodeKPVerticalbar
toScancode #{const SDL_SCANCODE_KP_DBLVERTICALBAR} = ScancodeKPDblverticalbar
toScancode #{const SDL_SCANCODE_KP_COLON} = ScancodeKPColon
toScancode #{const SDL_SCANCODE_KP_HASH} = ScancodeKPHash
toScancode #{const SDL_SCANCODE_KP_SPACE} = ScancodeKPSpace
toScancode #{const SDL_SCANCODE_KP_AT} = ScancodeKPAt
toScancode #{const SDL_SCANCODE_KP_EXCLAM} = ScancodeKPExclam
toScancode #{const SDL_SCANCODE_KP_MEMSTORE} = ScancodeKPMemstore
toScancode #{const SDL_SCANCODE_KP_MEMRECALL} = ScancodeKPMemrecall
toScancode #{const SDL_SCANCODE_KP_MEMCLEAR} = ScancodeKPMemclear
toScancode #{const SDL_SCANCODE_KP_MEMADD} = ScancodeKPMemadd
toScancode #{const SDL_SCANCODE_KP_MEMSUBTRACT} = ScancodeKPMemsubtract
toScancode #{const SDL_SCANCODE_KP_MEMMULTIPLY} = ScancodeKPMemmultiply
toScancode #{const SDL_SCANCODE_KP_MEMDIVIDE} = ScancodeKPMemdivide
toScancode #{const SDL_SCANCODE_KP_PLUSMINUS} = ScancodeKPPlusminus
toScancode #{const SDL_SCANCODE_KP_CLEAR} = ScancodeKPClear
toScancode #{const SDL_SCANCODE_KP_CLEARENTRY} = ScancodeKPClearentry
toScancode #{const SDL_SCANCODE_KP_BINARY} = ScancodeKPBinary
toScancode #{const SDL_SCANCODE_KP_OCTAL} = ScancodeKPOctal
toScancode #{const SDL_SCANCODE_KP_DECIMAL} = ScancodeKPDecimal
toScancode #{const SDL_SCANCODE_KP_HEXADECIMAL} = ScancodeKPHexadecimal
toScancode #{const SDL_SCANCODE_LCTRL} = ScancodeLCtrl
toScancode #{const SDL_SCANCODE_LSHIFT} = ScancodeLShift
toScancode #{const SDL_SCANCODE_LALT} = ScancodeLAlt
toScancode #{const SDL_SCANCODE_LGUI} = ScancodeLGUI
toScancode #{const SDL_SCANCODE_RCTRL} = ScancodeRCtrl
toScancode #{const SDL_SCANCODE_RSHIFT} = ScancodeRShift
toScancode #{const SDL_SCANCODE_RALT} = ScancodeRAlt
toScancode #{const SDL_SCANCODE_RGUI} = ScancodeRGUI
toScancode #{const SDL_SCANCODE_MODE} = ScancodeMode
toScancode #{const SDL_SCANCODE_AUDIONEXT} = ScancodeAudionext
toScancode #{const SDL_SCANCODE_AUDIOPREV} = ScancodeAudioprev
toScancode #{const SDL_SCANCODE_AUDIOSTOP} = ScancodeAudiostop
toScancode #{const SDL_SCANCODE_AUDIOPLAY} = ScancodeAudioplay
toScancode #{const SDL_SCANCODE_AUDIOMUTE} = ScancodeAudiomute
toScancode #{const SDL_SCANCODE_MEDIASELECT} = ScancodeMediaselect
toScancode #{const SDL_SCANCODE_WWW} = ScancodeWWW
toScancode #{const SDL_SCANCODE_MAIL} = ScancodeMail
toScancode #{const SDL_SCANCODE_CALCULATOR} = ScancodeCalculator
toScancode #{const SDL_SCANCODE_COMPUTER} = ScancodeComputer
toScancode #{const SDL_SCANCODE_AC_SEARCH} = ScancodeACSearch
toScancode #{const SDL_SCANCODE_AC_HOME} = ScancodeACHome
toScancode #{const SDL_SCANCODE_AC_BACK} = ScancodeACBack
toScancode #{const SDL_SCANCODE_AC_FORWARD} = ScancodeACForward
toScancode #{const SDL_SCANCODE_AC_STOP} = ScancodeACStop
toScancode #{const SDL_SCANCODE_AC_REFRESH} = ScancodeACRefresh
toScancode #{const SDL_SCANCODE_AC_BOOKMARKS} = ScancodeACBookmarks
toScancode #{const SDL_SCANCODE_BRIGHTNESSDOWN} = ScancodeBrightnessDown
toScancode #{const SDL_SCANCODE_BRIGHTNESSUP} = ScancodeBrightnessUp
toScancode #{const SDL_SCANCODE_DISPLAYSWITCH} = ScancodeDisplaySwitch
toScancode #{const SDL_SCANCODE_KBDILLUMTOGGLE} = ScancodeKBDILLUMTOGGLE
toScancode #{const SDL_SCANCODE_KBDILLUMDOWN} = ScancodeKBDIlluMDown
toScancode #{const SDL_SCANCODE_KBDILLUMUP} = ScancodeKBDIllumUp
toScancode #{const SDL_SCANCODE_EJECT} = ScancodeEject
toScancode #{const SDL_SCANCODE_SLEEP} = ScancodeSleep
toScancode #{const SDL_SCANCODE_APP1} = ScancodeApp1
toScancode #{const SDL_SCANCODE_APP2} = ScancodeApp2
toScancode _ = error "toScancode: unhandled scancode"

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

