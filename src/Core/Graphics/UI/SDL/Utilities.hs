-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.SDL.General
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Various small functions which makes the binding process easier.
-----------------------------------------------------------------------------
module Graphics.UI.SDL.Utilities where

import Data.Bits
import Foreign
import Data.Word

class UnsignedEnum a where
  succW :: a -> a
  predW :: a -> a
  toEnumW :: Word32 -> a
  fromEnumW :: a -> Word32
  enumFromToW :: a -> a -> [a]



intToBool :: Int -> IO Int -> IO Bool
intToBool err action
    = fmap (err/=) action

maybePtr :: (Storable a) => Maybe a -> (Ptr a -> IO b) -> IO b
maybePtr Nothing action = action nullPtr
maybePtr (Just v) action = with v action

toBitmask :: (Enum a) => [a] -> Int
toBitmask = foldr (.|.) 0 . map fromEnum

fromBitmask :: (Bounded a,Enum a) => Int -> [a]
fromBitmask mask = foldr worker [] lst
    where lst = [minBound .. maxBound]
          worker v
              = if (mask .&. fromEnum v) /= 0
                   then (:) v
                   else id

toBitmaskW :: (UnsignedEnum a) => [a] -> Word32
toBitmaskW = foldr (.|.) 0 . map fromEnumW

fromBitmaskW :: (Bounded a,UnsignedEnum a) => Word32 -> [a]
fromBitmaskW mask = foldr worker [] lst
    where lst = enumFromToW minBound maxBound
          worker v
              = if (mask .&. fromEnumW v) /= 0
                   then (:) v
                   else id


