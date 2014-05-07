#include "SDL.h"
module Graphics.UI.SDL.Power
    ( getPowerInfo
    , PowerState(..)
    , PowerInfo(..)
    ) where

import Control.Applicative
import Data.Time (DiffTime)
import Foreign

--------------------------------------------------------------------------------
data PowerState
  = PowerStateUnknown
  | PowerStateOnBattery
  | PowerStateNoBattery
  | PowerStateCharging
  | PowerStateCharged

decodePowerState :: #{type SDL_PowerState} -> PowerState
decodePowerState #{const SDL_POWERSTATE_UNKNOWN} = PowerStateUnknown
decodePowerState #{const SDL_POWERSTATE_ON_BATTERY} = PowerStateOnBattery
decodePowerState #{const SDL_POWERSTATE_NO_BATTERY} = PowerStateNoBattery
decodePowerState #{const SDL_POWERSTATE_CHARGING} = PowerStateCharging
decodePowerState #{const SDL_POWERSTATE_CHARGED} = PowerStateCharged
decodePowerState i = error $ "Unknown SDL_PowerState: " ++ show i

--------------------------------------------------------------------------------
data PowerInfo = PowerInfo { powerInfoState :: !PowerState
                           , powerInfoDurationLeft :: !(Maybe DiffTime)
                           , powerInfoPercentLeft :: !(Maybe #{type int})
                           }

foreign import ccall unsafe "SDL_GetPowerInfo"
  sdlGetPowerInfo :: Ptr #{type int} -> Ptr #{type int} -> IO #{type SDL_PowerState}

getPowerInfo :: IO PowerInfo
getPowerInfo =
  alloca $ \secPtr ->
  alloca $ \pctPtr ->
  PowerInfo <$> (decodePowerState <$> sdlGetPowerInfo secPtr pctPtr)
            <*> (whenPositive fromIntegral <$> peek secPtr)
            <*> (whenPositive id <$> peek pctPtr)
  where whenPositive f x | x < 0 = Nothing
                         | otherwise = Just (f x)
