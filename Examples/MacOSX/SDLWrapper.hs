{-# LANGUAGE ForeignFunctionInterface #-}
module SDLWrapper where
import Main
foreign export ccall "haskell_main" main :: IO ()
