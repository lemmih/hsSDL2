-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.SDL.Mixer.Types
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Graphics.UI.SDL.Mixer.Types where

import Foreign


data ChunkStruct
type Chunk = ForeignPtr ChunkStruct

data MusicStruct
type Music = ForeignPtr MusicStruct

type Channel = Int

data MusicType
    = MusicNone
    | MusicCmd
    | MusicWav
    | MusicMod
    | MusicMid
    | MusicOgg
    | MusicMp3
      deriving (Show,Eq,Ord,Enum,Bounded)

data Fading
    = NoFading
    | FadingOut
    | FadingIn
      deriving (Show,Eq,Ord,Enum,Bounded)
