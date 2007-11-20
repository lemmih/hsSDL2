#include "SDL.h"
#ifdef main
#undef main
#endif
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

import Foreign (Word16, Word32,
               Storable(poke, sizeOf, alignment, peekByteOff, pokeByteOff, peek))
import Data.Char (chr, ord)
import Prelude hiding (Enum(..))

import Graphics.UI.SDL.Utilities (Enum(..), toBitmask, fromBitmask)

data Keysym
  = Keysym
    { symKey :: SDLKey,
      symModifiers :: [Modifier],
      symUnicode :: Char}
    deriving (Show,Eq)

instance Storable Keysym where
    sizeOf = const #{size SDL_keysym}
    alignment = const 4
    poke ptr (Keysym key mods unicode)
        = do #{poke SDL_keysym, sym} ptr (fromEnum key)
             #{poke SDL_keysym, mod} ptr (toBitmask mods)
             #{poke SDL_keysym, unicode} ptr (fromIntegral (ord unicode) :: Word16)
    peek ptr
        = do sym <- #{peek SDL_keysym, sym} ptr
             mods <- #{peek SDL_keysym, mod} ptr
             uni <- #{peek SDL_keysym, unicode} ptr
             return $! Keysym (toEnum sym) (fromBitmask mods) (chr $ fromIntegral (uni::Word16))


data Modifier = KeyModNone
                 | KeyModLeftShift
                 | KeyModRightShift
                 | KeyModLeftCtrl
                 | KeyModRightCtrl
                 | KeyModLeftAlt
                 | KeyModRightAlt
                 | KeyModLeftMeta
                 | KeyModRightMeta
                 | KeyModNum
                 | KeyModCaps
                 | KeyModMode
                 | KeyModCtrl
                 | KeyModShift
                 | KeyModAlt
                 | KeyModMeta
    deriving (Eq, Ord, Show)
instance Bounded Modifier where
    minBound = KeyModNone
    maxBound = KeyModMode
instance Enum Modifier #{type SDLMod} where
      fromEnum KeyModNone = 0
      fromEnum KeyModLeftShift = 1
      fromEnum KeyModRightShift = 2
      fromEnum KeyModLeftCtrl = 64
      fromEnum KeyModRightCtrl = 128
      fromEnum KeyModLeftAlt = 256
      fromEnum KeyModRightAlt = 512
      fromEnum KeyModLeftMeta = 1024
      fromEnum KeyModRightMeta = 2048
      fromEnum KeyModNum = 4096
      fromEnum KeyModCaps = 8192
      fromEnum KeyModMode = 16384
      fromEnum KeyModCtrl = 192
      fromEnum KeyModShift = 3
      fromEnum KeyModAlt = 768
      fromEnum KeyModMeta = 3072
      toEnum 0 = KeyModNone
      toEnum 1 = KeyModLeftShift
      toEnum 2 = KeyModRightShift
      toEnum 64 = KeyModLeftCtrl
      toEnum 128 = KeyModRightCtrl
      toEnum 256 = KeyModLeftAlt
      toEnum 512 = KeyModRightAlt
      toEnum 1024 = KeyModLeftMeta
      toEnum 2048 = KeyModRightMeta
      toEnum 4096 = KeyModNum
      toEnum 8192 = KeyModCaps
      toEnum 16384 = KeyModMode
      toEnum 192 = KeyModCtrl
      toEnum 3 = KeyModShift
      toEnum 768 = KeyModAlt
      toEnum 3072 = KeyModMeta
      toEnum _ = error "Graphics.UI.SDL.Keysym.toEnum: bad argument"
      succ KeyModNone = KeyModLeftShift
      succ KeyModLeftShift = KeyModRightShift
      succ KeyModRightShift = KeyModLeftCtrl
      succ KeyModLeftCtrl = KeyModRightCtrl
      succ KeyModRightCtrl = KeyModLeftAlt
      succ KeyModLeftAlt = KeyModRightAlt
      succ KeyModRightAlt = KeyModLeftMeta
      succ KeyModLeftMeta = KeyModRightMeta
      succ KeyModRightMeta = KeyModNum
      succ KeyModNum = KeyModCaps
      succ KeyModCaps = KeyModMode
      succ KeyModMode = KeyModCtrl
      succ KeyModCtrl = KeyModShift
      succ KeyModShift = KeyModAlt
      succ KeyModAlt = KeyModMeta
      succ _ = error "Graphics.UI.SDL.Keysym.succ: bad argument"
      pred KeyModLeftShift = KeyModNone
      pred KeyModRightShift = KeyModLeftShift
      pred KeyModLeftCtrl = KeyModRightShift
      pred KeyModRightCtrl = KeyModLeftCtrl
      pred KeyModLeftAlt = KeyModRightCtrl
      pred KeyModRightAlt = KeyModLeftAlt
      pred KeyModLeftMeta = KeyModRightAlt
      pred KeyModRightMeta = KeyModLeftMeta
      pred KeyModNum = KeyModRightMeta
      pred KeyModCaps = KeyModNum
      pred KeyModMode = KeyModCaps
      pred KeyModCtrl = KeyModMode
      pred KeyModShift = KeyModCtrl
      pred KeyModAlt = KeyModShift
      pred KeyModMeta = KeyModAlt
      pred _ = error "Graphics.UI.SDL.Keysym.pred: bad argument"
      enumFromTo x y | x > y = []
                     | x == y = [y]
                     | True = x : enumFromTo (succ x) y

data SDLKey = SDLK_UNKNOWN
            | SDLK_FIRST
            | SDLK_BACKSPACE
            | SDLK_TAB
            | SDLK_CLEAR
            | SDLK_RETURN
            | SDLK_PAUSE
            | SDLK_ESCAPE
            | SDLK_SPACE
            | SDLK_EXCLAIM
            | SDLK_QUOTEDBL
            | SDLK_HASH
            | SDLK_DOLLAR
            | SDLK_AMPERSAND
            | SDLK_QUOTE
            | SDLK_LEFTPAREN
            | SDLK_RIGHTPAREN
            | SDLK_ASTERISK
            | SDLK_PLUS
            | SDLK_COMMA
            | SDLK_MINUS
            | SDLK_PERIOD
            | SDLK_SLASH
            | SDLK_0
            | SDLK_1
            | SDLK_2
            | SDLK_3
            | SDLK_4
            | SDLK_5
            | SDLK_6
            | SDLK_7
            | SDLK_8
            | SDLK_9
            | SDLK_COLON
            | SDLK_SEMICOLON
            | SDLK_LESS
            | SDLK_EQUALS
            | SDLK_GREATER
            | SDLK_QUESTION
            | SDLK_AT
            | SDLK_LEFTBRACKET
            | SDLK_BACKSLASH
            | SDLK_RIGHTBRACKET
            | SDLK_CARET
            | SDLK_UNDERSCORE
            | SDLK_BACKQUOTE
            | SDLK_a
            | SDLK_b
            | SDLK_c
            | SDLK_d
            | SDLK_e
            | SDLK_f
            | SDLK_g
            | SDLK_h
            | SDLK_i
            | SDLK_j
            | SDLK_k
            | SDLK_l
            | SDLK_m
            | SDLK_n
            | SDLK_o
            | SDLK_p
            | SDLK_q
            | SDLK_r
            | SDLK_s
            | SDLK_t
            | SDLK_u
            | SDLK_v
            | SDLK_w
            | SDLK_x
            | SDLK_y
            | SDLK_z
            | SDLK_DELETE
            | SDLK_WORLD_0
            | SDLK_WORLD_1
            | SDLK_WORLD_2
            | SDLK_WORLD_3
            | SDLK_WORLD_4
            | SDLK_WORLD_5
            | SDLK_WORLD_6
            | SDLK_WORLD_7
            | SDLK_WORLD_8
            | SDLK_WORLD_9
            | SDLK_WORLD_10
            | SDLK_WORLD_11
            | SDLK_WORLD_12
            | SDLK_WORLD_13
            | SDLK_WORLD_14
            | SDLK_WORLD_15
            | SDLK_WORLD_16
            | SDLK_WORLD_17
            | SDLK_WORLD_18
            | SDLK_WORLD_19
            | SDLK_WORLD_20
            | SDLK_WORLD_21
            | SDLK_WORLD_22
            | SDLK_WORLD_23
            | SDLK_WORLD_24
            | SDLK_WORLD_25
            | SDLK_WORLD_26
            | SDLK_WORLD_27
            | SDLK_WORLD_28
            | SDLK_WORLD_29
            | SDLK_WORLD_30
            | SDLK_WORLD_31
            | SDLK_WORLD_32
            | SDLK_WORLD_33
            | SDLK_WORLD_34
            | SDLK_WORLD_35
            | SDLK_WORLD_36
            | SDLK_WORLD_37
            | SDLK_WORLD_38
            | SDLK_WORLD_39
            | SDLK_WORLD_40
            | SDLK_WORLD_41
            | SDLK_WORLD_42
            | SDLK_WORLD_43
            | SDLK_WORLD_44
            | SDLK_WORLD_45
            | SDLK_WORLD_46
            | SDLK_WORLD_47
            | SDLK_WORLD_48
            | SDLK_WORLD_49
            | SDLK_WORLD_50
            | SDLK_WORLD_51
            | SDLK_WORLD_52
            | SDLK_WORLD_53
            | SDLK_WORLD_54
            | SDLK_WORLD_55
            | SDLK_WORLD_56
            | SDLK_WORLD_57
            | SDLK_WORLD_58
            | SDLK_WORLD_59
            | SDLK_WORLD_60
            | SDLK_WORLD_61
            | SDLK_WORLD_62
            | SDLK_WORLD_63
            | SDLK_WORLD_64
            | SDLK_WORLD_65
            | SDLK_WORLD_66
            | SDLK_WORLD_67
            | SDLK_WORLD_68
            | SDLK_WORLD_69
            | SDLK_WORLD_70
            | SDLK_WORLD_71
            | SDLK_WORLD_72
            | SDLK_WORLD_73
            | SDLK_WORLD_74
            | SDLK_WORLD_75
            | SDLK_WORLD_76
            | SDLK_WORLD_77
            | SDLK_WORLD_78
            | SDLK_WORLD_79
            | SDLK_WORLD_80
            | SDLK_WORLD_81
            | SDLK_WORLD_82
            | SDLK_WORLD_83
            | SDLK_WORLD_84
            | SDLK_WORLD_85
            | SDLK_WORLD_86
            | SDLK_WORLD_87
            | SDLK_WORLD_88
            | SDLK_WORLD_89
            | SDLK_WORLD_90
            | SDLK_WORLD_91
            | SDLK_WORLD_92
            | SDLK_WORLD_93
            | SDLK_WORLD_94
            | SDLK_WORLD_95
            | SDLK_KP0
            | SDLK_KP1
            | SDLK_KP2
            | SDLK_KP3
            | SDLK_KP4
            | SDLK_KP5
            | SDLK_KP6
            | SDLK_KP7
            | SDLK_KP8
            | SDLK_KP9
            | SDLK_KP_PERIOD
            | SDLK_KP_DIVIDE
            | SDLK_KP_MULTIPLY
            | SDLK_KP_MINUS
            | SDLK_KP_PLUS
            | SDLK_KP_ENTER
            | SDLK_KP_EQUALS
            | SDLK_UP
            | SDLK_DOWN
            | SDLK_RIGHT
            | SDLK_LEFT
            | SDLK_INSERT
            | SDLK_HOME
            | SDLK_END
            | SDLK_PAGEUP
            | SDLK_PAGEDOWN
            | SDLK_F1
            | SDLK_F2
            | SDLK_F3
            | SDLK_F4
            | SDLK_F5
            | SDLK_F6
            | SDLK_F7
            | SDLK_F8
            | SDLK_F9
            | SDLK_F10
            | SDLK_F11
            | SDLK_F12
            | SDLK_F13
            | SDLK_F14
            | SDLK_F15
            | SDLK_NUMLOCK
            | SDLK_CAPSLOCK
            | SDLK_SCROLLOCK
            | SDLK_RSHIFT
            | SDLK_LSHIFT
            | SDLK_RCTRL
            | SDLK_LCTRL
            | SDLK_RALT
            | SDLK_LALT
            | SDLK_RMETA
            | SDLK_LMETA
            | SDLK_LSUPER
            | SDLK_RSUPER
            | SDLK_MODE
            | SDLK_COMPOSE
            | SDLK_HELP
            | SDLK_PRINT
            | SDLK_SYSREQ
            | SDLK_BREAK
            | SDLK_MENU
            | SDLK_POWER
            | SDLK_EURO
            | SDLK_UNDO
            | SDLK_LAST
    deriving (Eq, Ord, Show)
instance Bounded SDLKey where
    minBound = SDLK_UNKNOWN
    maxBound = SDLK_LAST
instance Enum SDLKey #{type SDLMod} where
      fromEnum SDLK_UNKNOWN = 0
      fromEnum SDLK_FIRST = 0
      fromEnum SDLK_BACKSPACE = 8
      fromEnum SDLK_TAB = 9
      fromEnum SDLK_CLEAR = 12
      fromEnum SDLK_RETURN = 13
      fromEnum SDLK_PAUSE = 19
      fromEnum SDLK_ESCAPE = 27
      fromEnum SDLK_SPACE = 32
      fromEnum SDLK_EXCLAIM = 33
      fromEnum SDLK_QUOTEDBL = 34
      fromEnum SDLK_HASH = 35
      fromEnum SDLK_DOLLAR = 36
      fromEnum SDLK_AMPERSAND = 38
      fromEnum SDLK_QUOTE = 39
      fromEnum SDLK_LEFTPAREN = 40
      fromEnum SDLK_RIGHTPAREN = 41
      fromEnum SDLK_ASTERISK = 42
      fromEnum SDLK_PLUS = 43
      fromEnum SDLK_COMMA = 44
      fromEnum SDLK_MINUS = 45
      fromEnum SDLK_PERIOD = 46
      fromEnum SDLK_SLASH = 47
      fromEnum SDLK_0 = 48
      fromEnum SDLK_1 = 49
      fromEnum SDLK_2 = 50
      fromEnum SDLK_3 = 51
      fromEnum SDLK_4 = 52
      fromEnum SDLK_5 = 53
      fromEnum SDLK_6 = 54
      fromEnum SDLK_7 = 55
      fromEnum SDLK_8 = 56
      fromEnum SDLK_9 = 57
      fromEnum SDLK_COLON = 58
      fromEnum SDLK_SEMICOLON = 59
      fromEnum SDLK_LESS = 60
      fromEnum SDLK_EQUALS = 61
      fromEnum SDLK_GREATER = 62
      fromEnum SDLK_QUESTION = 63
      fromEnum SDLK_AT = 64
      fromEnum SDLK_LEFTBRACKET = 91
      fromEnum SDLK_BACKSLASH = 92
      fromEnum SDLK_RIGHTBRACKET = 93
      fromEnum SDLK_CARET = 94
      fromEnum SDLK_UNDERSCORE = 95
      fromEnum SDLK_BACKQUOTE = 96
      fromEnum SDLK_a = 97
      fromEnum SDLK_b = 98
      fromEnum SDLK_c = 99
      fromEnum SDLK_d = 100
      fromEnum SDLK_e = 101
      fromEnum SDLK_f = 102
      fromEnum SDLK_g = 103
      fromEnum SDLK_h = 104
      fromEnum SDLK_i = 105
      fromEnum SDLK_j = 106
      fromEnum SDLK_k = 107
      fromEnum SDLK_l = 108
      fromEnum SDLK_m = 109
      fromEnum SDLK_n = 110
      fromEnum SDLK_o = 111
      fromEnum SDLK_p = 112
      fromEnum SDLK_q = 113
      fromEnum SDLK_r = 114
      fromEnum SDLK_s = 115
      fromEnum SDLK_t = 116
      fromEnum SDLK_u = 117
      fromEnum SDLK_v = 118
      fromEnum SDLK_w = 119
      fromEnum SDLK_x = 120
      fromEnum SDLK_y = 121
      fromEnum SDLK_z = 122
      fromEnum SDLK_DELETE = 127
      fromEnum SDLK_WORLD_0 = 160
      fromEnum SDLK_WORLD_1 = 161
      fromEnum SDLK_WORLD_2 = 162
      fromEnum SDLK_WORLD_3 = 163
      fromEnum SDLK_WORLD_4 = 164
      fromEnum SDLK_WORLD_5 = 165
      fromEnum SDLK_WORLD_6 = 166
      fromEnum SDLK_WORLD_7 = 167
      fromEnum SDLK_WORLD_8 = 168
      fromEnum SDLK_WORLD_9 = 169
      fromEnum SDLK_WORLD_10 = 170
      fromEnum SDLK_WORLD_11 = 171
      fromEnum SDLK_WORLD_12 = 172
      fromEnum SDLK_WORLD_13 = 173
      fromEnum SDLK_WORLD_14 = 174
      fromEnum SDLK_WORLD_15 = 175
      fromEnum SDLK_WORLD_16 = 176
      fromEnum SDLK_WORLD_17 = 177
      fromEnum SDLK_WORLD_18 = 178
      fromEnum SDLK_WORLD_19 = 179
      fromEnum SDLK_WORLD_20 = 180
      fromEnum SDLK_WORLD_21 = 181
      fromEnum SDLK_WORLD_22 = 182
      fromEnum SDLK_WORLD_23 = 183
      fromEnum SDLK_WORLD_24 = 184
      fromEnum SDLK_WORLD_25 = 185
      fromEnum SDLK_WORLD_26 = 186
      fromEnum SDLK_WORLD_27 = 187
      fromEnum SDLK_WORLD_28 = 188
      fromEnum SDLK_WORLD_29 = 189
      fromEnum SDLK_WORLD_30 = 190
      fromEnum SDLK_WORLD_31 = 191
      fromEnum SDLK_WORLD_32 = 192
      fromEnum SDLK_WORLD_33 = 193
      fromEnum SDLK_WORLD_34 = 194
      fromEnum SDLK_WORLD_35 = 195
      fromEnum SDLK_WORLD_36 = 196
      fromEnum SDLK_WORLD_37 = 197
      fromEnum SDLK_WORLD_38 = 198
      fromEnum SDLK_WORLD_39 = 199
      fromEnum SDLK_WORLD_40 = 200
      fromEnum SDLK_WORLD_41 = 201
      fromEnum SDLK_WORLD_42 = 202
      fromEnum SDLK_WORLD_43 = 203
      fromEnum SDLK_WORLD_44 = 204
      fromEnum SDLK_WORLD_45 = 205
      fromEnum SDLK_WORLD_46 = 206
      fromEnum SDLK_WORLD_47 = 207
      fromEnum SDLK_WORLD_48 = 208
      fromEnum SDLK_WORLD_49 = 209
      fromEnum SDLK_WORLD_50 = 210
      fromEnum SDLK_WORLD_51 = 211
      fromEnum SDLK_WORLD_52 = 212
      fromEnum SDLK_WORLD_53 = 213
      fromEnum SDLK_WORLD_54 = 214
      fromEnum SDLK_WORLD_55 = 215
      fromEnum SDLK_WORLD_56 = 216
      fromEnum SDLK_WORLD_57 = 217
      fromEnum SDLK_WORLD_58 = 218
      fromEnum SDLK_WORLD_59 = 219
      fromEnum SDLK_WORLD_60 = 220
      fromEnum SDLK_WORLD_61 = 221
      fromEnum SDLK_WORLD_62 = 222
      fromEnum SDLK_WORLD_63 = 223
      fromEnum SDLK_WORLD_64 = 224
      fromEnum SDLK_WORLD_65 = 225
      fromEnum SDLK_WORLD_66 = 226
      fromEnum SDLK_WORLD_67 = 227
      fromEnum SDLK_WORLD_68 = 228
      fromEnum SDLK_WORLD_69 = 229
      fromEnum SDLK_WORLD_70 = 230
      fromEnum SDLK_WORLD_71 = 231
      fromEnum SDLK_WORLD_72 = 232
      fromEnum SDLK_WORLD_73 = 233
      fromEnum SDLK_WORLD_74 = 234
      fromEnum SDLK_WORLD_75 = 235
      fromEnum SDLK_WORLD_76 = 236
      fromEnum SDLK_WORLD_77 = 237
      fromEnum SDLK_WORLD_78 = 238
      fromEnum SDLK_WORLD_79 = 239
      fromEnum SDLK_WORLD_80 = 240
      fromEnum SDLK_WORLD_81 = 241
      fromEnum SDLK_WORLD_82 = 242
      fromEnum SDLK_WORLD_83 = 243
      fromEnum SDLK_WORLD_84 = 244
      fromEnum SDLK_WORLD_85 = 245
      fromEnum SDLK_WORLD_86 = 246
      fromEnum SDLK_WORLD_87 = 247
      fromEnum SDLK_WORLD_88 = 248
      fromEnum SDLK_WORLD_89 = 249
      fromEnum SDLK_WORLD_90 = 250
      fromEnum SDLK_WORLD_91 = 251
      fromEnum SDLK_WORLD_92 = 252
      fromEnum SDLK_WORLD_93 = 253
      fromEnum SDLK_WORLD_94 = 254
      fromEnum SDLK_WORLD_95 = 255
      fromEnum SDLK_KP0 = 256
      fromEnum SDLK_KP1 = 257
      fromEnum SDLK_KP2 = 258
      fromEnum SDLK_KP3 = 259
      fromEnum SDLK_KP4 = 260
      fromEnum SDLK_KP5 = 261
      fromEnum SDLK_KP6 = 262
      fromEnum SDLK_KP7 = 263
      fromEnum SDLK_KP8 = 264
      fromEnum SDLK_KP9 = 265
      fromEnum SDLK_KP_PERIOD = 266
      fromEnum SDLK_KP_DIVIDE = 267
      fromEnum SDLK_KP_MULTIPLY = 268
      fromEnum SDLK_KP_MINUS = 269
      fromEnum SDLK_KP_PLUS = 270
      fromEnum SDLK_KP_ENTER = 271
      fromEnum SDLK_KP_EQUALS = 272
      fromEnum SDLK_UP = 273
      fromEnum SDLK_DOWN = 274
      fromEnum SDLK_RIGHT = 275
      fromEnum SDLK_LEFT = 276
      fromEnum SDLK_INSERT = 277
      fromEnum SDLK_HOME = 278
      fromEnum SDLK_END = 279
      fromEnum SDLK_PAGEUP = 280
      fromEnum SDLK_PAGEDOWN = 281
      fromEnum SDLK_F1 = 282
      fromEnum SDLK_F2 = 283
      fromEnum SDLK_F3 = 284
      fromEnum SDLK_F4 = 285
      fromEnum SDLK_F5 = 286
      fromEnum SDLK_F6 = 287
      fromEnum SDLK_F7 = 288
      fromEnum SDLK_F8 = 289
      fromEnum SDLK_F9 = 290
      fromEnum SDLK_F10 = 291
      fromEnum SDLK_F11 = 292
      fromEnum SDLK_F12 = 293
      fromEnum SDLK_F13 = 294
      fromEnum SDLK_F14 = 295
      fromEnum SDLK_F15 = 296
      fromEnum SDLK_NUMLOCK = 300
      fromEnum SDLK_CAPSLOCK = 301
      fromEnum SDLK_SCROLLOCK = 302
      fromEnum SDLK_RSHIFT = 303
      fromEnum SDLK_LSHIFT = 304
      fromEnum SDLK_RCTRL = 305
      fromEnum SDLK_LCTRL = 306
      fromEnum SDLK_RALT = 307
      fromEnum SDLK_LALT = 308
      fromEnum SDLK_RMETA = 309
      fromEnum SDLK_LMETA = 310
      fromEnum SDLK_LSUPER = 311
      fromEnum SDLK_RSUPER = 312
      fromEnum SDLK_MODE = 313
      fromEnum SDLK_COMPOSE = 314
      fromEnum SDLK_HELP = 315
      fromEnum SDLK_PRINT = 316
      fromEnum SDLK_SYSREQ = 317
      fromEnum SDLK_BREAK = 318
      fromEnum SDLK_MENU = 319
      fromEnum SDLK_POWER = 320
      fromEnum SDLK_EURO = 321
      fromEnum SDLK_UNDO = 322
      fromEnum SDLK_LAST = 323
      toEnum 0 = SDLK_UNKNOWN
      toEnum 8 = SDLK_BACKSPACE
      toEnum 9 = SDLK_TAB
      toEnum 12 = SDLK_CLEAR
      toEnum 13 = SDLK_RETURN
      toEnum 19 = SDLK_PAUSE
      toEnum 27 = SDLK_ESCAPE
      toEnum 32 = SDLK_SPACE
      toEnum 33 = SDLK_EXCLAIM
      toEnum 34 = SDLK_QUOTEDBL
      toEnum 35 = SDLK_HASH
      toEnum 36 = SDLK_DOLLAR
      toEnum 38 = SDLK_AMPERSAND
      toEnum 39 = SDLK_QUOTE
      toEnum 40 = SDLK_LEFTPAREN
      toEnum 41 = SDLK_RIGHTPAREN
      toEnum 42 = SDLK_ASTERISK
      toEnum 43 = SDLK_PLUS
      toEnum 44 = SDLK_COMMA
      toEnum 45 = SDLK_MINUS
      toEnum 46 = SDLK_PERIOD
      toEnum 47 = SDLK_SLASH
      toEnum 48 = SDLK_0
      toEnum 49 = SDLK_1
      toEnum 50 = SDLK_2
      toEnum 51 = SDLK_3
      toEnum 52 = SDLK_4
      toEnum 53 = SDLK_5
      toEnum 54 = SDLK_6
      toEnum 55 = SDLK_7
      toEnum 56 = SDLK_8
      toEnum 57 = SDLK_9
      toEnum 58 = SDLK_COLON
      toEnum 59 = SDLK_SEMICOLON
      toEnum 60 = SDLK_LESS
      toEnum 61 = SDLK_EQUALS
      toEnum 62 = SDLK_GREATER
      toEnum 63 = SDLK_QUESTION
      toEnum 64 = SDLK_AT
      toEnum 91 = SDLK_LEFTBRACKET
      toEnum 92 = SDLK_BACKSLASH
      toEnum 93 = SDLK_RIGHTBRACKET
      toEnum 94 = SDLK_CARET
      toEnum 95 = SDLK_UNDERSCORE
      toEnum 96 = SDLK_BACKQUOTE
      toEnum 97 = SDLK_a
      toEnum 98 = SDLK_b
      toEnum 99 = SDLK_c
      toEnum 100 = SDLK_d
      toEnum 101 = SDLK_e
      toEnum 102 = SDLK_f
      toEnum 103 = SDLK_g
      toEnum 104 = SDLK_h
      toEnum 105 = SDLK_i
      toEnum 106 = SDLK_j
      toEnum 107 = SDLK_k
      toEnum 108 = SDLK_l
      toEnum 109 = SDLK_m
      toEnum 110 = SDLK_n
      toEnum 111 = SDLK_o
      toEnum 112 = SDLK_p
      toEnum 113 = SDLK_q
      toEnum 114 = SDLK_r
      toEnum 115 = SDLK_s
      toEnum 116 = SDLK_t
      toEnum 117 = SDLK_u
      toEnum 118 = SDLK_v
      toEnum 119 = SDLK_w
      toEnum 120 = SDLK_x
      toEnum 121 = SDLK_y
      toEnum 122 = SDLK_z
      toEnum 127 = SDLK_DELETE
      toEnum 160 = SDLK_WORLD_0
      toEnum 161 = SDLK_WORLD_1
      toEnum 162 = SDLK_WORLD_2
      toEnum 163 = SDLK_WORLD_3
      toEnum 164 = SDLK_WORLD_4
      toEnum 165 = SDLK_WORLD_5
      toEnum 166 = SDLK_WORLD_6
      toEnum 167 = SDLK_WORLD_7
      toEnum 168 = SDLK_WORLD_8
      toEnum 169 = SDLK_WORLD_9
      toEnum 170 = SDLK_WORLD_10
      toEnum 171 = SDLK_WORLD_11
      toEnum 172 = SDLK_WORLD_12
      toEnum 173 = SDLK_WORLD_13
      toEnum 174 = SDLK_WORLD_14
      toEnum 175 = SDLK_WORLD_15
      toEnum 176 = SDLK_WORLD_16
      toEnum 177 = SDLK_WORLD_17
      toEnum 178 = SDLK_WORLD_18
      toEnum 179 = SDLK_WORLD_19
      toEnum 180 = SDLK_WORLD_20
      toEnum 181 = SDLK_WORLD_21
      toEnum 182 = SDLK_WORLD_22
      toEnum 183 = SDLK_WORLD_23
      toEnum 184 = SDLK_WORLD_24
      toEnum 185 = SDLK_WORLD_25
      toEnum 186 = SDLK_WORLD_26
      toEnum 187 = SDLK_WORLD_27
      toEnum 188 = SDLK_WORLD_28
      toEnum 189 = SDLK_WORLD_29
      toEnum 190 = SDLK_WORLD_30
      toEnum 191 = SDLK_WORLD_31
      toEnum 192 = SDLK_WORLD_32
      toEnum 193 = SDLK_WORLD_33
      toEnum 194 = SDLK_WORLD_34
      toEnum 195 = SDLK_WORLD_35
      toEnum 196 = SDLK_WORLD_36
      toEnum 197 = SDLK_WORLD_37
      toEnum 198 = SDLK_WORLD_38
      toEnum 199 = SDLK_WORLD_39
      toEnum 200 = SDLK_WORLD_40
      toEnum 201 = SDLK_WORLD_41
      toEnum 202 = SDLK_WORLD_42
      toEnum 203 = SDLK_WORLD_43
      toEnum 204 = SDLK_WORLD_44
      toEnum 205 = SDLK_WORLD_45
      toEnum 206 = SDLK_WORLD_46
      toEnum 207 = SDLK_WORLD_47
      toEnum 208 = SDLK_WORLD_48
      toEnum 209 = SDLK_WORLD_49
      toEnum 210 = SDLK_WORLD_50
      toEnum 211 = SDLK_WORLD_51
      toEnum 212 = SDLK_WORLD_52
      toEnum 213 = SDLK_WORLD_53
      toEnum 214 = SDLK_WORLD_54
      toEnum 215 = SDLK_WORLD_55
      toEnum 216 = SDLK_WORLD_56
      toEnum 217 = SDLK_WORLD_57
      toEnum 218 = SDLK_WORLD_58
      toEnum 219 = SDLK_WORLD_59
      toEnum 220 = SDLK_WORLD_60
      toEnum 221 = SDLK_WORLD_61
      toEnum 222 = SDLK_WORLD_62
      toEnum 223 = SDLK_WORLD_63
      toEnum 224 = SDLK_WORLD_64
      toEnum 225 = SDLK_WORLD_65
      toEnum 226 = SDLK_WORLD_66
      toEnum 227 = SDLK_WORLD_67
      toEnum 228 = SDLK_WORLD_68
      toEnum 229 = SDLK_WORLD_69
      toEnum 230 = SDLK_WORLD_70
      toEnum 231 = SDLK_WORLD_71
      toEnum 232 = SDLK_WORLD_72
      toEnum 233 = SDLK_WORLD_73
      toEnum 234 = SDLK_WORLD_74
      toEnum 235 = SDLK_WORLD_75
      toEnum 236 = SDLK_WORLD_76
      toEnum 237 = SDLK_WORLD_77
      toEnum 238 = SDLK_WORLD_78
      toEnum 239 = SDLK_WORLD_79
      toEnum 240 = SDLK_WORLD_80
      toEnum 241 = SDLK_WORLD_81
      toEnum 242 = SDLK_WORLD_82
      toEnum 243 = SDLK_WORLD_83
      toEnum 244 = SDLK_WORLD_84
      toEnum 245 = SDLK_WORLD_85
      toEnum 246 = SDLK_WORLD_86
      toEnum 247 = SDLK_WORLD_87
      toEnum 248 = SDLK_WORLD_88
      toEnum 249 = SDLK_WORLD_89
      toEnum 250 = SDLK_WORLD_90
      toEnum 251 = SDLK_WORLD_91
      toEnum 252 = SDLK_WORLD_92
      toEnum 253 = SDLK_WORLD_93
      toEnum 254 = SDLK_WORLD_94
      toEnum 255 = SDLK_WORLD_95
      toEnum 256 = SDLK_KP0
      toEnum 257 = SDLK_KP1
      toEnum 258 = SDLK_KP2
      toEnum 259 = SDLK_KP3
      toEnum 260 = SDLK_KP4
      toEnum 261 = SDLK_KP5
      toEnum 262 = SDLK_KP6
      toEnum 263 = SDLK_KP7
      toEnum 264 = SDLK_KP8
      toEnum 265 = SDLK_KP9
      toEnum 266 = SDLK_KP_PERIOD
      toEnum 267 = SDLK_KP_DIVIDE
      toEnum 268 = SDLK_KP_MULTIPLY
      toEnum 269 = SDLK_KP_MINUS
      toEnum 270 = SDLK_KP_PLUS
      toEnum 271 = SDLK_KP_ENTER
      toEnum 272 = SDLK_KP_EQUALS
      toEnum 273 = SDLK_UP
      toEnum 274 = SDLK_DOWN
      toEnum 275 = SDLK_RIGHT
      toEnum 276 = SDLK_LEFT
      toEnum 277 = SDLK_INSERT
      toEnum 278 = SDLK_HOME
      toEnum 279 = SDLK_END
      toEnum 280 = SDLK_PAGEUP
      toEnum 281 = SDLK_PAGEDOWN
      toEnum 282 = SDLK_F1
      toEnum 283 = SDLK_F2
      toEnum 284 = SDLK_F3
      toEnum 285 = SDLK_F4
      toEnum 286 = SDLK_F5
      toEnum 287 = SDLK_F6
      toEnum 288 = SDLK_F7
      toEnum 289 = SDLK_F8
      toEnum 290 = SDLK_F9
      toEnum 291 = SDLK_F10
      toEnum 292 = SDLK_F11
      toEnum 293 = SDLK_F12
      toEnum 294 = SDLK_F13
      toEnum 295 = SDLK_F14
      toEnum 296 = SDLK_F15
      toEnum 300 = SDLK_NUMLOCK
      toEnum 301 = SDLK_CAPSLOCK
      toEnum 302 = SDLK_SCROLLOCK
      toEnum 303 = SDLK_RSHIFT
      toEnum 304 = SDLK_LSHIFT
      toEnum 305 = SDLK_RCTRL
      toEnum 306 = SDLK_LCTRL
      toEnum 307 = SDLK_RALT
      toEnum 308 = SDLK_LALT
      toEnum 309 = SDLK_RMETA
      toEnum 310 = SDLK_LMETA
      toEnum 311 = SDLK_LSUPER
      toEnum 312 = SDLK_RSUPER
      toEnum 313 = SDLK_MODE
      toEnum 314 = SDLK_COMPOSE
      toEnum 315 = SDLK_HELP
      toEnum 316 = SDLK_PRINT
      toEnum 317 = SDLK_SYSREQ
      toEnum 318 = SDLK_BREAK
      toEnum 319 = SDLK_MENU
      toEnum 320 = SDLK_POWER
      toEnum 321 = SDLK_EURO
      toEnum 322 = SDLK_UNDO
      toEnum 323 = SDLK_LAST
      toEnum _ = error "Graphics.UI.SDL.Keysym.toEnum: bad argument"
      succ SDLK_UNKNOWN = SDLK_FIRST
      succ SDLK_FIRST = SDLK_BACKSPACE
      succ SDLK_BACKSPACE = SDLK_TAB
      succ SDLK_TAB = SDLK_CLEAR
      succ SDLK_CLEAR = SDLK_RETURN
      succ SDLK_RETURN = SDLK_PAUSE
      succ SDLK_PAUSE = SDLK_ESCAPE
      succ SDLK_ESCAPE = SDLK_SPACE
      succ SDLK_SPACE = SDLK_EXCLAIM
      succ SDLK_EXCLAIM = SDLK_QUOTEDBL
      succ SDLK_QUOTEDBL = SDLK_HASH
      succ SDLK_HASH = SDLK_DOLLAR
      succ SDLK_DOLLAR = SDLK_AMPERSAND
      succ SDLK_AMPERSAND = SDLK_QUOTE
      succ SDLK_QUOTE = SDLK_LEFTPAREN
      succ SDLK_LEFTPAREN = SDLK_RIGHTPAREN
      succ SDLK_RIGHTPAREN = SDLK_ASTERISK
      succ SDLK_ASTERISK = SDLK_PLUS
      succ SDLK_PLUS = SDLK_COMMA
      succ SDLK_COMMA = SDLK_MINUS
      succ SDLK_MINUS = SDLK_PERIOD
      succ SDLK_PERIOD = SDLK_SLASH
      succ SDLK_SLASH = SDLK_0
      succ SDLK_0 = SDLK_1
      succ SDLK_1 = SDLK_2
      succ SDLK_2 = SDLK_3
      succ SDLK_3 = SDLK_4
      succ SDLK_4 = SDLK_5
      succ SDLK_5 = SDLK_6
      succ SDLK_6 = SDLK_7
      succ SDLK_7 = SDLK_8
      succ SDLK_8 = SDLK_9
      succ SDLK_9 = SDLK_COLON
      succ SDLK_COLON = SDLK_SEMICOLON
      succ SDLK_SEMICOLON = SDLK_LESS
      succ SDLK_LESS = SDLK_EQUALS
      succ SDLK_EQUALS = SDLK_GREATER
      succ SDLK_GREATER = SDLK_QUESTION
      succ SDLK_QUESTION = SDLK_AT
      succ SDLK_AT = SDLK_LEFTBRACKET
      succ SDLK_LEFTBRACKET = SDLK_BACKSLASH
      succ SDLK_BACKSLASH = SDLK_RIGHTBRACKET
      succ SDLK_RIGHTBRACKET = SDLK_CARET
      succ SDLK_CARET = SDLK_UNDERSCORE
      succ SDLK_UNDERSCORE = SDLK_BACKQUOTE
      succ SDLK_BACKQUOTE = SDLK_a
      succ SDLK_a = SDLK_b
      succ SDLK_b = SDLK_c
      succ SDLK_c = SDLK_d
      succ SDLK_d = SDLK_e
      succ SDLK_e = SDLK_f
      succ SDLK_f = SDLK_g
      succ SDLK_g = SDLK_h
      succ SDLK_h = SDLK_i
      succ SDLK_i = SDLK_j
      succ SDLK_j = SDLK_k
      succ SDLK_k = SDLK_l
      succ SDLK_l = SDLK_m
      succ SDLK_m = SDLK_n
      succ SDLK_n = SDLK_o
      succ SDLK_o = SDLK_p
      succ SDLK_p = SDLK_q
      succ SDLK_q = SDLK_r
      succ SDLK_r = SDLK_s
      succ SDLK_s = SDLK_t
      succ SDLK_t = SDLK_u
      succ SDLK_u = SDLK_v
      succ SDLK_v = SDLK_w
      succ SDLK_w = SDLK_x
      succ SDLK_x = SDLK_y
      succ SDLK_y = SDLK_z
      succ SDLK_z = SDLK_DELETE
      succ SDLK_DELETE = SDLK_WORLD_0
      succ SDLK_WORLD_0 = SDLK_WORLD_1
      succ SDLK_WORLD_1 = SDLK_WORLD_2
      succ SDLK_WORLD_2 = SDLK_WORLD_3
      succ SDLK_WORLD_3 = SDLK_WORLD_4
      succ SDLK_WORLD_4 = SDLK_WORLD_5
      succ SDLK_WORLD_5 = SDLK_WORLD_6
      succ SDLK_WORLD_6 = SDLK_WORLD_7
      succ SDLK_WORLD_7 = SDLK_WORLD_8
      succ SDLK_WORLD_8 = SDLK_WORLD_9
      succ SDLK_WORLD_9 = SDLK_WORLD_10
      succ SDLK_WORLD_10 = SDLK_WORLD_11
      succ SDLK_WORLD_11 = SDLK_WORLD_12
      succ SDLK_WORLD_12 = SDLK_WORLD_13
      succ SDLK_WORLD_13 = SDLK_WORLD_14
      succ SDLK_WORLD_14 = SDLK_WORLD_15
      succ SDLK_WORLD_15 = SDLK_WORLD_16
      succ SDLK_WORLD_16 = SDLK_WORLD_17
      succ SDLK_WORLD_17 = SDLK_WORLD_18
      succ SDLK_WORLD_18 = SDLK_WORLD_19
      succ SDLK_WORLD_19 = SDLK_WORLD_20
      succ SDLK_WORLD_20 = SDLK_WORLD_21
      succ SDLK_WORLD_21 = SDLK_WORLD_22
      succ SDLK_WORLD_22 = SDLK_WORLD_23
      succ SDLK_WORLD_23 = SDLK_WORLD_24
      succ SDLK_WORLD_24 = SDLK_WORLD_25
      succ SDLK_WORLD_25 = SDLK_WORLD_26
      succ SDLK_WORLD_26 = SDLK_WORLD_27
      succ SDLK_WORLD_27 = SDLK_WORLD_28
      succ SDLK_WORLD_28 = SDLK_WORLD_29
      succ SDLK_WORLD_29 = SDLK_WORLD_30
      succ SDLK_WORLD_30 = SDLK_WORLD_31
      succ SDLK_WORLD_31 = SDLK_WORLD_32
      succ SDLK_WORLD_32 = SDLK_WORLD_33
      succ SDLK_WORLD_33 = SDLK_WORLD_34
      succ SDLK_WORLD_34 = SDLK_WORLD_35
      succ SDLK_WORLD_35 = SDLK_WORLD_36
      succ SDLK_WORLD_36 = SDLK_WORLD_37
      succ SDLK_WORLD_37 = SDLK_WORLD_38
      succ SDLK_WORLD_38 = SDLK_WORLD_39
      succ SDLK_WORLD_39 = SDLK_WORLD_40
      succ SDLK_WORLD_40 = SDLK_WORLD_41
      succ SDLK_WORLD_41 = SDLK_WORLD_42
      succ SDLK_WORLD_42 = SDLK_WORLD_43
      succ SDLK_WORLD_43 = SDLK_WORLD_44
      succ SDLK_WORLD_44 = SDLK_WORLD_45
      succ SDLK_WORLD_45 = SDLK_WORLD_46
      succ SDLK_WORLD_46 = SDLK_WORLD_47
      succ SDLK_WORLD_47 = SDLK_WORLD_48
      succ SDLK_WORLD_48 = SDLK_WORLD_49
      succ SDLK_WORLD_49 = SDLK_WORLD_50
      succ SDLK_WORLD_50 = SDLK_WORLD_51
      succ SDLK_WORLD_51 = SDLK_WORLD_52
      succ SDLK_WORLD_52 = SDLK_WORLD_53
      succ SDLK_WORLD_53 = SDLK_WORLD_54
      succ SDLK_WORLD_54 = SDLK_WORLD_55
      succ SDLK_WORLD_55 = SDLK_WORLD_56
      succ SDLK_WORLD_56 = SDLK_WORLD_57
      succ SDLK_WORLD_57 = SDLK_WORLD_58
      succ SDLK_WORLD_58 = SDLK_WORLD_59
      succ SDLK_WORLD_59 = SDLK_WORLD_60
      succ SDLK_WORLD_60 = SDLK_WORLD_61
      succ SDLK_WORLD_61 = SDLK_WORLD_62
      succ SDLK_WORLD_62 = SDLK_WORLD_63
      succ SDLK_WORLD_63 = SDLK_WORLD_64
      succ SDLK_WORLD_64 = SDLK_WORLD_65
      succ SDLK_WORLD_65 = SDLK_WORLD_66
      succ SDLK_WORLD_66 = SDLK_WORLD_67
      succ SDLK_WORLD_67 = SDLK_WORLD_68
      succ SDLK_WORLD_68 = SDLK_WORLD_69
      succ SDLK_WORLD_69 = SDLK_WORLD_70
      succ SDLK_WORLD_70 = SDLK_WORLD_71
      succ SDLK_WORLD_71 = SDLK_WORLD_72
      succ SDLK_WORLD_72 = SDLK_WORLD_73
      succ SDLK_WORLD_73 = SDLK_WORLD_74
      succ SDLK_WORLD_74 = SDLK_WORLD_75
      succ SDLK_WORLD_75 = SDLK_WORLD_76
      succ SDLK_WORLD_76 = SDLK_WORLD_77
      succ SDLK_WORLD_77 = SDLK_WORLD_78
      succ SDLK_WORLD_78 = SDLK_WORLD_79
      succ SDLK_WORLD_79 = SDLK_WORLD_80
      succ SDLK_WORLD_80 = SDLK_WORLD_81
      succ SDLK_WORLD_81 = SDLK_WORLD_82
      succ SDLK_WORLD_82 = SDLK_WORLD_83
      succ SDLK_WORLD_83 = SDLK_WORLD_84
      succ SDLK_WORLD_84 = SDLK_WORLD_85
      succ SDLK_WORLD_85 = SDLK_WORLD_86
      succ SDLK_WORLD_86 = SDLK_WORLD_87
      succ SDLK_WORLD_87 = SDLK_WORLD_88
      succ SDLK_WORLD_88 = SDLK_WORLD_89
      succ SDLK_WORLD_89 = SDLK_WORLD_90
      succ SDLK_WORLD_90 = SDLK_WORLD_91
      succ SDLK_WORLD_91 = SDLK_WORLD_92
      succ SDLK_WORLD_92 = SDLK_WORLD_93
      succ SDLK_WORLD_93 = SDLK_WORLD_94
      succ SDLK_WORLD_94 = SDLK_WORLD_95
      succ SDLK_WORLD_95 = SDLK_KP0
      succ SDLK_KP0 = SDLK_KP1
      succ SDLK_KP1 = SDLK_KP2
      succ SDLK_KP2 = SDLK_KP3
      succ SDLK_KP3 = SDLK_KP4
      succ SDLK_KP4 = SDLK_KP5
      succ SDLK_KP5 = SDLK_KP6
      succ SDLK_KP6 = SDLK_KP7
      succ SDLK_KP7 = SDLK_KP8
      succ SDLK_KP8 = SDLK_KP9
      succ SDLK_KP9 = SDLK_KP_PERIOD
      succ SDLK_KP_PERIOD = SDLK_KP_DIVIDE
      succ SDLK_KP_DIVIDE = SDLK_KP_MULTIPLY
      succ SDLK_KP_MULTIPLY = SDLK_KP_MINUS
      succ SDLK_KP_MINUS = SDLK_KP_PLUS
      succ SDLK_KP_PLUS = SDLK_KP_ENTER
      succ SDLK_KP_ENTER = SDLK_KP_EQUALS
      succ SDLK_KP_EQUALS = SDLK_UP
      succ SDLK_UP = SDLK_DOWN
      succ SDLK_DOWN = SDLK_RIGHT
      succ SDLK_RIGHT = SDLK_LEFT
      succ SDLK_LEFT = SDLK_INSERT
      succ SDLK_INSERT = SDLK_HOME
      succ SDLK_HOME = SDLK_END
      succ SDLK_END = SDLK_PAGEUP
      succ SDLK_PAGEUP = SDLK_PAGEDOWN
      succ SDLK_PAGEDOWN = SDLK_F1
      succ SDLK_F1 = SDLK_F2
      succ SDLK_F2 = SDLK_F3
      succ SDLK_F3 = SDLK_F4
      succ SDLK_F4 = SDLK_F5
      succ SDLK_F5 = SDLK_F6
      succ SDLK_F6 = SDLK_F7
      succ SDLK_F7 = SDLK_F8
      succ SDLK_F8 = SDLK_F9
      succ SDLK_F9 = SDLK_F10
      succ SDLK_F10 = SDLK_F11
      succ SDLK_F11 = SDLK_F12
      succ SDLK_F12 = SDLK_F13
      succ SDLK_F13 = SDLK_F14
      succ SDLK_F14 = SDLK_F15
      succ SDLK_F15 = SDLK_NUMLOCK
      succ SDLK_NUMLOCK = SDLK_CAPSLOCK
      succ SDLK_CAPSLOCK = SDLK_SCROLLOCK
      succ SDLK_SCROLLOCK = SDLK_RSHIFT
      succ SDLK_RSHIFT = SDLK_LSHIFT
      succ SDLK_LSHIFT = SDLK_RCTRL
      succ SDLK_RCTRL = SDLK_LCTRL
      succ SDLK_LCTRL = SDLK_RALT
      succ SDLK_RALT = SDLK_LALT
      succ SDLK_LALT = SDLK_RMETA
      succ SDLK_RMETA = SDLK_LMETA
      succ SDLK_LMETA = SDLK_LSUPER
      succ SDLK_LSUPER = SDLK_RSUPER
      succ SDLK_RSUPER = SDLK_MODE
      succ SDLK_MODE = SDLK_COMPOSE
      succ SDLK_COMPOSE = SDLK_HELP
      succ SDLK_HELP = SDLK_PRINT
      succ SDLK_PRINT = SDLK_SYSREQ
      succ SDLK_SYSREQ = SDLK_BREAK
      succ SDLK_BREAK = SDLK_MENU
      succ SDLK_MENU = SDLK_POWER
      succ SDLK_POWER = SDLK_EURO
      succ SDLK_EURO = SDLK_UNDO
      succ SDLK_UNDO = SDLK_LAST
      succ _ = error "Graphics.UI.SDL.Keysym.succ: bad argument"
      pred SDLK_FIRST = SDLK_UNKNOWN
      pred SDLK_BACKSPACE = SDLK_FIRST
      pred SDLK_TAB = SDLK_BACKSPACE
      pred SDLK_CLEAR = SDLK_TAB
      pred SDLK_RETURN = SDLK_CLEAR
      pred SDLK_PAUSE = SDLK_RETURN
      pred SDLK_ESCAPE = SDLK_PAUSE
      pred SDLK_SPACE = SDLK_ESCAPE
      pred SDLK_EXCLAIM = SDLK_SPACE
      pred SDLK_QUOTEDBL = SDLK_EXCLAIM
      pred SDLK_HASH = SDLK_QUOTEDBL
      pred SDLK_DOLLAR = SDLK_HASH
      pred SDLK_AMPERSAND = SDLK_DOLLAR
      pred SDLK_QUOTE = SDLK_AMPERSAND
      pred SDLK_LEFTPAREN = SDLK_QUOTE
      pred SDLK_RIGHTPAREN = SDLK_LEFTPAREN
      pred SDLK_ASTERISK = SDLK_RIGHTPAREN
      pred SDLK_PLUS = SDLK_ASTERISK
      pred SDLK_COMMA = SDLK_PLUS
      pred SDLK_MINUS = SDLK_COMMA
      pred SDLK_PERIOD = SDLK_MINUS
      pred SDLK_SLASH = SDLK_PERIOD
      pred SDLK_0 = SDLK_SLASH
      pred SDLK_1 = SDLK_0
      pred SDLK_2 = SDLK_1
      pred SDLK_3 = SDLK_2
      pred SDLK_4 = SDLK_3
      pred SDLK_5 = SDLK_4
      pred SDLK_6 = SDLK_5
      pred SDLK_7 = SDLK_6
      pred SDLK_8 = SDLK_7
      pred SDLK_9 = SDLK_8
      pred SDLK_COLON = SDLK_9
      pred SDLK_SEMICOLON = SDLK_COLON
      pred SDLK_LESS = SDLK_SEMICOLON
      pred SDLK_EQUALS = SDLK_LESS
      pred SDLK_GREATER = SDLK_EQUALS
      pred SDLK_QUESTION = SDLK_GREATER
      pred SDLK_AT = SDLK_QUESTION
      pred SDLK_LEFTBRACKET = SDLK_AT
      pred SDLK_BACKSLASH = SDLK_LEFTBRACKET
      pred SDLK_RIGHTBRACKET = SDLK_BACKSLASH
      pred SDLK_CARET = SDLK_RIGHTBRACKET
      pred SDLK_UNDERSCORE = SDLK_CARET
      pred SDLK_BACKQUOTE = SDLK_UNDERSCORE
      pred SDLK_a = SDLK_BACKQUOTE
      pred SDLK_b = SDLK_a
      pred SDLK_c = SDLK_b
      pred SDLK_d = SDLK_c
      pred SDLK_e = SDLK_d
      pred SDLK_f = SDLK_e
      pred SDLK_g = SDLK_f
      pred SDLK_h = SDLK_g
      pred SDLK_i = SDLK_h
      pred SDLK_j = SDLK_i
      pred SDLK_k = SDLK_j
      pred SDLK_l = SDLK_k
      pred SDLK_m = SDLK_l
      pred SDLK_n = SDLK_m
      pred SDLK_o = SDLK_n
      pred SDLK_p = SDLK_o
      pred SDLK_q = SDLK_p
      pred SDLK_r = SDLK_q
      pred SDLK_s = SDLK_r
      pred SDLK_t = SDLK_s
      pred SDLK_u = SDLK_t
      pred SDLK_v = SDLK_u
      pred SDLK_w = SDLK_v
      pred SDLK_x = SDLK_w
      pred SDLK_y = SDLK_x
      pred SDLK_z = SDLK_y
      pred SDLK_DELETE = SDLK_z
      pred SDLK_WORLD_0 = SDLK_DELETE
      pred SDLK_WORLD_1 = SDLK_WORLD_0
      pred SDLK_WORLD_2 = SDLK_WORLD_1
      pred SDLK_WORLD_3 = SDLK_WORLD_2
      pred SDLK_WORLD_4 = SDLK_WORLD_3
      pred SDLK_WORLD_5 = SDLK_WORLD_4
      pred SDLK_WORLD_6 = SDLK_WORLD_5
      pred SDLK_WORLD_7 = SDLK_WORLD_6
      pred SDLK_WORLD_8 = SDLK_WORLD_7
      pred SDLK_WORLD_9 = SDLK_WORLD_8
      pred SDLK_WORLD_10 = SDLK_WORLD_9
      pred SDLK_WORLD_11 = SDLK_WORLD_10
      pred SDLK_WORLD_12 = SDLK_WORLD_11
      pred SDLK_WORLD_13 = SDLK_WORLD_12
      pred SDLK_WORLD_14 = SDLK_WORLD_13
      pred SDLK_WORLD_15 = SDLK_WORLD_14
      pred SDLK_WORLD_16 = SDLK_WORLD_15
      pred SDLK_WORLD_17 = SDLK_WORLD_16
      pred SDLK_WORLD_18 = SDLK_WORLD_17
      pred SDLK_WORLD_19 = SDLK_WORLD_18
      pred SDLK_WORLD_20 = SDLK_WORLD_19
      pred SDLK_WORLD_21 = SDLK_WORLD_20
      pred SDLK_WORLD_22 = SDLK_WORLD_21
      pred SDLK_WORLD_23 = SDLK_WORLD_22
      pred SDLK_WORLD_24 = SDLK_WORLD_23
      pred SDLK_WORLD_25 = SDLK_WORLD_24
      pred SDLK_WORLD_26 = SDLK_WORLD_25
      pred SDLK_WORLD_27 = SDLK_WORLD_26
      pred SDLK_WORLD_28 = SDLK_WORLD_27
      pred SDLK_WORLD_29 = SDLK_WORLD_28
      pred SDLK_WORLD_30 = SDLK_WORLD_29
      pred SDLK_WORLD_31 = SDLK_WORLD_30
      pred SDLK_WORLD_32 = SDLK_WORLD_31
      pred SDLK_WORLD_33 = SDLK_WORLD_32
      pred SDLK_WORLD_34 = SDLK_WORLD_33
      pred SDLK_WORLD_35 = SDLK_WORLD_34
      pred SDLK_WORLD_36 = SDLK_WORLD_35
      pred SDLK_WORLD_37 = SDLK_WORLD_36
      pred SDLK_WORLD_38 = SDLK_WORLD_37
      pred SDLK_WORLD_39 = SDLK_WORLD_38
      pred SDLK_WORLD_40 = SDLK_WORLD_39
      pred SDLK_WORLD_41 = SDLK_WORLD_40
      pred SDLK_WORLD_42 = SDLK_WORLD_41
      pred SDLK_WORLD_43 = SDLK_WORLD_42
      pred SDLK_WORLD_44 = SDLK_WORLD_43
      pred SDLK_WORLD_45 = SDLK_WORLD_44
      pred SDLK_WORLD_46 = SDLK_WORLD_45
      pred SDLK_WORLD_47 = SDLK_WORLD_46
      pred SDLK_WORLD_48 = SDLK_WORLD_47
      pred SDLK_WORLD_49 = SDLK_WORLD_48
      pred SDLK_WORLD_50 = SDLK_WORLD_49
      pred SDLK_WORLD_51 = SDLK_WORLD_50
      pred SDLK_WORLD_52 = SDLK_WORLD_51
      pred SDLK_WORLD_53 = SDLK_WORLD_52
      pred SDLK_WORLD_54 = SDLK_WORLD_53
      pred SDLK_WORLD_55 = SDLK_WORLD_54
      pred SDLK_WORLD_56 = SDLK_WORLD_55
      pred SDLK_WORLD_57 = SDLK_WORLD_56
      pred SDLK_WORLD_58 = SDLK_WORLD_57
      pred SDLK_WORLD_59 = SDLK_WORLD_58
      pred SDLK_WORLD_60 = SDLK_WORLD_59
      pred SDLK_WORLD_61 = SDLK_WORLD_60
      pred SDLK_WORLD_62 = SDLK_WORLD_61
      pred SDLK_WORLD_63 = SDLK_WORLD_62
      pred SDLK_WORLD_64 = SDLK_WORLD_63
      pred SDLK_WORLD_65 = SDLK_WORLD_64
      pred SDLK_WORLD_66 = SDLK_WORLD_65
      pred SDLK_WORLD_67 = SDLK_WORLD_66
      pred SDLK_WORLD_68 = SDLK_WORLD_67
      pred SDLK_WORLD_69 = SDLK_WORLD_68
      pred SDLK_WORLD_70 = SDLK_WORLD_69
      pred SDLK_WORLD_71 = SDLK_WORLD_70
      pred SDLK_WORLD_72 = SDLK_WORLD_71
      pred SDLK_WORLD_73 = SDLK_WORLD_72
      pred SDLK_WORLD_74 = SDLK_WORLD_73
      pred SDLK_WORLD_75 = SDLK_WORLD_74
      pred SDLK_WORLD_76 = SDLK_WORLD_75
      pred SDLK_WORLD_77 = SDLK_WORLD_76
      pred SDLK_WORLD_78 = SDLK_WORLD_77
      pred SDLK_WORLD_79 = SDLK_WORLD_78
      pred SDLK_WORLD_80 = SDLK_WORLD_79
      pred SDLK_WORLD_81 = SDLK_WORLD_80
      pred SDLK_WORLD_82 = SDLK_WORLD_81
      pred SDLK_WORLD_83 = SDLK_WORLD_82
      pred SDLK_WORLD_84 = SDLK_WORLD_83
      pred SDLK_WORLD_85 = SDLK_WORLD_84
      pred SDLK_WORLD_86 = SDLK_WORLD_85
      pred SDLK_WORLD_87 = SDLK_WORLD_86
      pred SDLK_WORLD_88 = SDLK_WORLD_87
      pred SDLK_WORLD_89 = SDLK_WORLD_88
      pred SDLK_WORLD_90 = SDLK_WORLD_89
      pred SDLK_WORLD_91 = SDLK_WORLD_90
      pred SDLK_WORLD_92 = SDLK_WORLD_91
      pred SDLK_WORLD_93 = SDLK_WORLD_92
      pred SDLK_WORLD_94 = SDLK_WORLD_93
      pred SDLK_WORLD_95 = SDLK_WORLD_94
      pred SDLK_KP0 = SDLK_WORLD_95
      pred SDLK_KP1 = SDLK_KP0
      pred SDLK_KP2 = SDLK_KP1
      pred SDLK_KP3 = SDLK_KP2
      pred SDLK_KP4 = SDLK_KP3
      pred SDLK_KP5 = SDLK_KP4
      pred SDLK_KP6 = SDLK_KP5
      pred SDLK_KP7 = SDLK_KP6
      pred SDLK_KP8 = SDLK_KP7
      pred SDLK_KP9 = SDLK_KP8
      pred SDLK_KP_PERIOD = SDLK_KP9
      pred SDLK_KP_DIVIDE = SDLK_KP_PERIOD
      pred SDLK_KP_MULTIPLY = SDLK_KP_DIVIDE
      pred SDLK_KP_MINUS = SDLK_KP_MULTIPLY
      pred SDLK_KP_PLUS = SDLK_KP_MINUS
      pred SDLK_KP_ENTER = SDLK_KP_PLUS
      pred SDLK_KP_EQUALS = SDLK_KP_ENTER
      pred SDLK_UP = SDLK_KP_EQUALS
      pred SDLK_DOWN = SDLK_UP
      pred SDLK_RIGHT = SDLK_DOWN
      pred SDLK_LEFT = SDLK_RIGHT
      pred SDLK_INSERT = SDLK_LEFT
      pred SDLK_HOME = SDLK_INSERT
      pred SDLK_END = SDLK_HOME
      pred SDLK_PAGEUP = SDLK_END
      pred SDLK_PAGEDOWN = SDLK_PAGEUP
      pred SDLK_F1 = SDLK_PAGEDOWN
      pred SDLK_F2 = SDLK_F1
      pred SDLK_F3 = SDLK_F2
      pred SDLK_F4 = SDLK_F3
      pred SDLK_F5 = SDLK_F4
      pred SDLK_F6 = SDLK_F5
      pred SDLK_F7 = SDLK_F6
      pred SDLK_F8 = SDLK_F7
      pred SDLK_F9 = SDLK_F8
      pred SDLK_F10 = SDLK_F9
      pred SDLK_F11 = SDLK_F10
      pred SDLK_F12 = SDLK_F11
      pred SDLK_F13 = SDLK_F12
      pred SDLK_F14 = SDLK_F13
      pred SDLK_F15 = SDLK_F14
      pred SDLK_NUMLOCK = SDLK_F15
      pred SDLK_CAPSLOCK = SDLK_NUMLOCK
      pred SDLK_SCROLLOCK = SDLK_CAPSLOCK
      pred SDLK_RSHIFT = SDLK_SCROLLOCK
      pred SDLK_LSHIFT = SDLK_RSHIFT
      pred SDLK_RCTRL = SDLK_LSHIFT
      pred SDLK_LCTRL = SDLK_RCTRL
      pred SDLK_RALT = SDLK_LCTRL
      pred SDLK_LALT = SDLK_RALT
      pred SDLK_RMETA = SDLK_LALT
      pred SDLK_LMETA = SDLK_RMETA
      pred SDLK_LSUPER = SDLK_LMETA
      pred SDLK_RSUPER = SDLK_LSUPER
      pred SDLK_MODE = SDLK_RSUPER
      pred SDLK_COMPOSE = SDLK_MODE
      pred SDLK_HELP = SDLK_COMPOSE
      pred SDLK_PRINT = SDLK_HELP
      pred SDLK_SYSREQ = SDLK_PRINT
      pred SDLK_BREAK = SDLK_SYSREQ
      pred SDLK_MENU = SDLK_BREAK
      pred SDLK_POWER = SDLK_MENU
      pred SDLK_EURO = SDLK_POWER
      pred SDLK_UNDO = SDLK_EURO
      pred SDLK_LAST = SDLK_UNDO
      pred _ = error "Graphics.UI.SDL.Keysym.pred: bad argument"
      enumFromTo x y | x > y = []
                     | x == y = [y]
                     | True = x : enumFromTo (succ x) y
