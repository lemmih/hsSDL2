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

import Foreign (Bits((.|.), (.&.)))
import Foreign.C (CInt)

import Prelude hiding (Enum(..))

class Enum a b | a -> b where
  succ :: a -> a
  pred :: a -> a
  toEnum :: b -> a
  fromEnum :: a -> b
  enumFromTo :: a -> a -> [a]



intToBool :: Int -> IO Int -> IO Bool
intToBool err action
    = fmap (err/=) action

toBitmask :: (Enum a b,Bits b,Num b) => [a] -> b
toBitmask = foldr (.|.) 0 . map fromEnum

fromBitmask :: (Bounded a,Enum a b,Bits b,Num b) => b -> [a]
fromBitmask mask = foldr worker [] lst
    where lst = enumFromTo minBound maxBound
          worker v
              = if (mask .&. fromEnum v) /= 0
                   then (:) v
                   else id
{-
toBitmaskW :: (UnsignedEnum a) => [a] -> Word32
toBitmaskW = foldr (.|.) 0 . map fromEnumW

fromBitmaskW :: (Bounded a,UnsignedEnum a) => Word32 -> [a]
fromBitmaskW mask = foldr worker [] lst
    where lst = enumFromToW minBound maxBound
          worker v
              = if (mask .&. fromEnumW v) /= 0
                   then (:) v
                   else id

-}

fromCInt :: Num a => CInt -> a
fromCInt = fromIntegral

toCInt :: Int -> CInt
toCInt = fromIntegral
