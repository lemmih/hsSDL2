{- 

FunGEN - Functional Game Engine
http://www.cin.ufpe.br/~haskell/fungen
Copyright (C) 2002  Andre Furtado <awbf@cin.ufpe.br>

This code is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

This FunGEn module contains some auxiliary functions.

-}
{-
  Further hacked on by Lemmih
-}

module BmpAux where

import Graphics.UI.GLUT
import Random

shiftLeft :: String -> Int -> String
shiftLeft a 0 = a
shiftLeft (_:as) n = shiftLeft(as ++ "0") (n-1)
shiftLeft _ _ = []

toDecimal :: String -> GLsizei
toDecimal a = toDecimalAux (reverse a) 32

toDecimalAux :: String -> GLsizei -> GLsizei
toDecimalAux [] _ = 0
toDecimalAux _ 0 = 0
toDecimalAux (a:as) n
                | a == '0' = toDecimalAux as (n-1)
                | otherwise = pow2 (32 - n) + toDecimalAux as (n-1)
                
pow2 :: GLsizei -> GLsizei
pow2 0 = 1
pow2 n = 2 * pow2(n-1)

toBinary :: Int -> String
toBinary n
        | n < 2 = show n
        | otherwise = toBinary (n `div` 2) ++ (show (n `mod` 2))
        
make0 :: Int -> String
make0 0 = []
make0 n = '0':(make0 (n-1))

ord2 :: Char -> GLubyte
ord2 a = (toEnum.fromEnum) a

dropGLsizei                :: GLsizei -> [a] -> [a]
dropGLsizei 0 xs            = xs
dropGLsizei _ []            = []
dropGLsizei n (_:xs) | n>0  = dropGLsizei (n-1) xs
dropGLsizei _ _ = error "Fun_Aux.dropGLsizei error: negative argument"

