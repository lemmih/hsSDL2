module Graphics.UI.SDL.StringUtilities where

escapePrintf :: String -> String
escapePrintf s = flip concatMap s $ \c -> case c of
                                            '%' -> "%%"
                                            _   -> [c]
