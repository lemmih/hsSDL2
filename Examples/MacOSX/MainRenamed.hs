{-# LANGUAGE ForeignFunctionInterface #-}
module MainRenamed where
import Main (main)
foreign export ccall "haskell_main" main :: IO ()
