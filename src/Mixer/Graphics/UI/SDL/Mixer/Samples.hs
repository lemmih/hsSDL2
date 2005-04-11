-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.SDL.Mixer.Samples
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Graphics.UI.SDL.Mixer.Samples
    ( mkFinalizedChunk
    , maxVolume
    , tryLoadWAV
    , loadWAV
    , volumeChunk
    ) where

import Foreign(Ptr, FunPtr, nullPtr, withForeignPtr, newForeignPtr)
import Foreign.C(withCString, CString)

import Graphics.UI.SDL.Mixer.Types(Chunk, ChunkStruct)
import Graphics.UI.SDL.General(unwrapMaybe)

maxVolume :: Int
maxVolume = 128

-- void Mix_FreeChunk(Mix_Chunk *chunk)
foreign import ccall unsafe "&Mix_FreeChunk" mixFreeChunkFinal :: FunPtr (Ptr ChunkStruct -> IO ())
mkFinalizedChunk :: Ptr ChunkStruct -> IO Chunk
mkFinalizedChunk = newForeignPtr mixFreeChunkFinal

-- Mix_Chunk *Mix_LoadWAV(char *file)
foreign import ccall unsafe "Mix_LoadWAV" mixLoadWAV :: CString -> IO (Ptr ChunkStruct)
tryLoadWAV :: FilePath -> IO (Maybe Chunk)
tryLoadWAV string
    = withCString string $ \cString ->
      do chunk <- mixLoadWAV cString
         if chunk == nullPtr
            then return Nothing
            else fmap Just (mkFinalizedChunk chunk)

loadWAV :: FilePath -> IO Chunk
loadWAV path = unwrapMaybe "Mix_LoadWAV" (tryLoadWAV path)

-- int Mix_VolumeChunk(Mix_Chunk *chunk, int volume)
foreign import ccall unsafe "Mix_VolumeChunk" mixVolumeChunk :: Ptr ChunkStruct -> Int -> IO Int
volumeChunk :: Chunk -> Int -> IO Int
volumeChunk chunk volume = withForeignPtr chunk (\ptr -> mixVolumeChunk ptr volume)

