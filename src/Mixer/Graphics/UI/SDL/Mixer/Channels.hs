-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.SDL.Mixer.Channels
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Graphics.UI.SDL.Mixer.Channels
    ( allocateChannels
    , volume
    , tryPlayChannel
    , playChannel
    , tryFadeInChannel
    , fadeInChannel
    , tryFadeInChannelTimed
    , fadeInChannelTimed
    , pause
    , resume
    , haltChannel
    , expireChannel
    , fadeOutChannel
    , isChannelPlaying
    , numChannelsPlaying
    , isChannelPaused
    , numChannelsPaused
    , fadingChannel
    , getChunk
    ) where

import Graphics.UI.SDL.Mixer.Types
import Graphics.UI.SDL.General

import Foreign

-- int Mix_AllocateChannels(int numchans)
foreign import ccall unsafe "Mix_AllocateChannels" allocateChannels :: Int -> IO Int

-- int Mix_Volume(int channel, int volume)
foreign import ccall unsafe "Mix_Volume" volume :: Int -> Int -> IO Int

tryPlayChannel :: Channel -> Chunk -> Int -> IO Int
tryPlayChannel channel chunk loops
    = tryPlayChannelTimed channel chunk loops (-1)

playChannel :: Channel -> Chunk -> Int -> IO Int
playChannel channel chunk loops
    = playChannelTimed channel chunk loops (-1)

-- int Mix_PlayChannelTimed(int channel, Mix_Chunk *chunk, int loops, int ticks)
foreign import ccall unsafe "Mix_PlayChannelTimed" mixPlayChannelTimed
    :: Int -> Ptr ChunkStruct -> Int -> Int -> IO Int
tryPlayChannelTimed :: Channel -> Chunk -> Int -> Int -> IO Int
tryPlayChannelTimed channel chunk loops ticks
    = withForeignPtr chunk $ \chunkPtr ->
      mixPlayChannelTimed channel chunkPtr loops ticks

playChannelTimed :: Channel -> Chunk -> Int -> Int -> IO Int
playChannelTimed channel chunk loops ticks
    = unwrapInt (/=(-1)) "Mix_PlayChannelTimed"
                (tryPlayChannelTimed channel chunk loops ticks)

tryFadeInChannel :: Channel -> Chunk -> Int -> Int -> IO Int
tryFadeInChannel channel chunk loops ms
    = tryFadeInChannelTimed channel chunk loops ms (-1)

fadeInChannel :: Channel -> Chunk -> Int -> Int -> IO Int
fadeInChannel channel chunk loops ms
    = fadeInChannelTimed channel chunk loops ms (-1)

-- int Mix_FadeInChannelTimed(int channel, Mix_Chunk *chunk, int loops, int ms, int ticks)
foreign import ccall unsafe "Mix_FadeInChannelTimed" mixFadeInChannelTimed
    :: Int -> Ptr ChunkStruct -> Int -> Int -> Int -> IO Int
tryFadeInChannelTimed :: Channel -> Chunk -> Int -> Int -> Int -> IO Int
tryFadeInChannelTimed channel chunk loops ms ticks
    = withForeignPtr chunk $ \chunkPtr ->
      mixFadeInChannelTimed channel chunkPtr loops ms ticks

fadeInChannelTimed :: Channel -> Chunk -> Int -> Int -> Int -> IO Int
fadeInChannelTimed channel chunk loops ms ticks
    = unwrapInt (/=(-1)) "Mix_FadeInChannelTimed"
                (tryFadeInChannelTimed channel chunk loops ms ticks)


-- void Mix_Pause(int channel)
foreign import ccall unsafe "Mix_Pause" pause :: Channel -> IO ()

-- void Mix_Resume(int channel)
foreign import ccall unsafe "Mix_Resume" resume :: Channel -> IO ()

-- int Mix_HaltChannel(int channel)
foreign import ccall unsafe "Mix_HaltChannel" mixHaltChannel :: Int -> IO Int
haltChannel :: Channel -> IO ()
haltChannel channel = mixHaltChannel channel >> return ()

-- int Mix_ExpireChannel(int channel, int ticks)
foreign import ccall unsafe "Mix_ExpireChannel" expireChannel :: Channel -> Int -> IO Int

-- int Mix_FadeOutChannel(int channel, int ms)
foreign import ccall unsafe "Mix_FadeOutChannel" fadeOutChannel :: Channel -> Int -> IO Int

-- int Mix_Playing(int channel)
foreign import ccall unsafe "Mix_Playing" mixPlaying :: Int -> IO Int

isChannelPlaying :: Channel -> IO Bool
isChannelPlaying = fmap toBool . mixPlaying

numChannelsPlaying :: IO Int
numChannelsPlaying = mixPlaying (-1)

-- int Mix_Paused(int channel)
foreign import ccall unsafe "Mix_Paused" mixPaused :: Int -> IO Int

isChannelPaused :: Channel -> IO Bool
isChannelPaused = fmap toBool . mixPaused

numChannelsPaused :: IO Int
numChannelsPaused = mixPaused (-1)

-- Mix_Fading Mix_FadingChannel(int which)
foreign import ccall unsafe "Mix_FadingChannel" mixFadingChannel :: Int -> IO Int
fadingChannel :: Channel -> IO Fading
fadingChannel = fmap toEnum . mixFadingChannel

-- Mix_Chunk *Mix_GetChunk(int channel)
foreign import ccall unsafe "Mix_GetChunk" mixGetChunk :: Int -> IO (Ptr ChunkStruct)
getChunk :: Channel -> IO Chunk
getChunk ch = newForeignPtr_ =<< mixGetChunk ch