-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.SDL.Mixer.Music
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Graphics.UI.SDL.Mixer.Music
    ( freeMusic
    , tryLoadMUS
    , loadMUS
    , tryPlayMusic
    , playMusic
    , tryFadeInMusic
    , fadeInMusic
    , tryFadeInMusicPos
    , fadeInMusicPos
    , setMusicVolume
    , getMusicVolume
    , modifyMusicVolume
    , pauseMusic
    , resumeMusic
    , rewindMusic
    , trySetMusicPosition
    , setMusicPosition
    , trySetMusicCmd
    , setMusicCmd
    , disableMusicCmd
    , haltMusic
    , tryFadeOutMusic
    , fadeOutMusic
    , getMusicType
    , playingMusic
    , pausedMusic
    , fadingMusic
    ) where


import Foreign(Ptr, FunPtr, nullPtr, toBool, withForeignPtr, newForeignPtr)
import Foreign.C(withCString, CString)

import Graphics.UI.SDL.Mixer.Types(Fading, MusicType, Music, MusicStruct)
import Graphics.UI.SDL.General(unwrapMaybe, unwrapBool)

-- void Mix_FreeMusic(Mix_Music *music)
foreign import ccall unsafe "&Mix_FreeMusic" mixFreeMusicFinal :: FunPtr (Ptr MusicStruct -> IO ())

mkFinalizedMusic :: Ptr MusicStruct -> IO Music
mkFinalizedMusic = newForeignPtr mixFreeMusicFinal

foreign import ccall unsafe "Mix_FreeMusic" mixFreeMusic :: Ptr MusicStruct -> IO ()
freeMusic :: Music -> IO ()
freeMusic music = withForeignPtr music mixFreeMusic

-- Mix_Music *Mix_LoadMUS(const char *file)
foreign import ccall unsafe "Mix_LoadMUS" mixLoadMUS :: CString -> IO (Ptr MusicStruct)
tryLoadMUS :: FilePath -> IO (Maybe Music)
tryLoadMUS path
    = withCString path $ \cPath ->
      do music <- mixLoadMUS cPath
         if music == nullPtr
            then return Nothing
            else fmap Just (mkFinalizedMusic music)

loadMUS :: FilePath -> IO Music
loadMUS = unwrapMaybe "Mix_LoadMUS" . tryLoadMUS

-- int Mix_PlayMusic(Mix_Music *music, int loops)
foreign import ccall unsafe "Mix_PlayMusic" mixPlayMusic :: Ptr MusicStruct -> Int -> IO Int
tryPlayMusic :: Music -> Int -> IO Bool
tryPlayMusic music loops
    = withForeignPtr music $ \musicPtr ->
      fmap (==0) (mixPlayMusic musicPtr loops)

playMusic :: Music -> Int -> IO ()
playMusic music loops = unwrapBool "Mix_PlayMusic" (tryPlayMusic music loops)

tryFadeInMusic :: Music -> Int -> Int -> IO Bool
tryFadeInMusic music loops ms
    = tryFadeInMusicPos music loops ms 0

fadeInMusic :: Music -> Int -> Int -> IO ()
fadeInMusic music loops ms
    = fadeInMusicPos music loops ms 0

-- int Mix_FadeInMusicPos(Mix_Music *music, int loops, int ms, double position)
foreign import ccall unsafe "Mix_FadeInMusicPos" mixFadeInMusicPos
    :: Ptr MusicStruct -> Int -> Int -> Double -> IO Int
tryFadeInMusicPos :: Music -> Int -> Int -> Double -> IO Bool
tryFadeInMusicPos music loops ms pos
    = withForeignPtr music $ \musicPtr ->
      fmap (==0) (mixFadeInMusicPos musicPtr loops ms pos)

fadeInMusicPos :: Music -> Int -> Int -> Double -> IO ()
fadeInMusicPos music loops ms pos
    = unwrapBool "Mix_FadeInMusic" (tryFadeInMusicPos music loops ms pos)

-- int Mix_VolumeMusic(int volume)
foreign import ccall unsafe "Mix_VolumeMusic" mixVolumeMusic :: Int -> IO Int

setMusicVolume :: Int -> IO ()
setMusicVolume volume = mixVolumeMusic volume >> return ()

getMusicVolume :: IO Int
getMusicVolume = mixVolumeMusic (-1)

modifyMusicVolume :: (Int -> Int) -> IO ()
modifyMusicVolume fn = getMusicVolume >>= setMusicVolume . fn

-- void Mix_PauseMusic()
foreign import ccall unsafe "Mix_PauseMusic" pauseMusic :: IO ()

-- void Mix_ResumeMusic()
foreign import ccall unsafe "Mix_ResumeMusic" resumeMusic :: IO ()

-- void Mix_RewindMusic()
foreign import ccall unsafe "Mix_RewindMusic" rewindMusic :: IO ()

-- int Mix_SetMusicPosition(double position)
foreign import ccall unsafe "Mix_SetMusicPosition" mixSetMusicPosition :: Double -> IO Int

trySetMusicPosition :: Double -> IO Bool
trySetMusicPosition = fmap (==0) . mixSetMusicPosition

setMusicPosition :: Double -> IO ()
setMusicPosition = unwrapBool "Mix_SetMusicPosition" . trySetMusicPosition

-- int Mix_SetMusicCMD(const char *command)
foreign import ccall unsafe "Mix_SetMusicCMD" mixSetMusicCmd :: CString -> IO Int

trySetMusicCmd :: String -> IO Bool
trySetMusicCmd cmd
    = withCString cmd $ \cString ->
      fmap (==0) (mixSetMusicCmd cString)

setMusicCmd :: String -> IO ()
setMusicCmd cmd = unwrapBool "Mix_SetMusicCMD" (trySetMusicCmd cmd)

disableMusicCmd :: IO ()
disableMusicCmd = mixSetMusicCmd nullPtr >> return ()

-- int Mix_HaltMusic()
foreign import ccall unsafe "Mix_HaltMusic" mixHaltMusic :: IO Int
haltMusic :: IO ()
haltMusic = mixHaltMusic >> return ()

-- int Mix_FadeOutMusic(int ms)
foreign import ccall unsafe "Mix_FadeOutMusic" mixFadeOutMusic :: Int -> IO Int
tryFadeOutMusic :: Int -> IO Bool
tryFadeOutMusic ms = fmap (==1) (mixFadeOutMusic ms)

fadeOutMusic :: Int -> IO ()
fadeOutMusic ms = unwrapBool "Mix_FadeOutMusic" (tryFadeOutMusic ms)

-- Mix_MusicType Mix_GetMusicType(const Mix_Music *music)
foreign import ccall unsafe "Mix_GetMusicType" mixGetMusicType :: Ptr MusicStruct -> IO Int

getMusicType :: Maybe Music -> IO MusicType
getMusicType mbMusic
    = withMusic mbMusic $ \musicPtr ->
      fmap toEnum (mixGetMusicType musicPtr)
    where withMusic Nothing action = action nullPtr
          withMusic (Just music) action = withForeignPtr music action

-- int Mix_PlayingMusic()
foreign import ccall unsafe "Mix_PlayingMusic" mixPlayingMusic :: IO Int

playingMusic :: IO Bool
playingMusic = fmap toBool mixPlayingMusic

-- int Mix_PausedMusic()
foreign import ccall unsafe "Mix_PausedMusic" mixPausedMusic :: IO Int

pausedMusic :: IO Bool
pausedMusic = fmap toBool mixPausedMusic

-- Mix_Fading Mix_FadingMusic()
foreign import ccall unsafe "Mix_FadingMusic" mixFadingMusic :: IO Int

fadingMusic :: IO Fading
fadingMusic = fmap toEnum mixFadingMusic


