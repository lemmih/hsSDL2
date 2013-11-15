{-# LANGUAGE TemplateHaskell #-}
module Graphics.UI.SDL.Meta (makeEnum) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Foreign.Storable
import Language.Haskell.TH

-- data ty = constructors deriving (Show, Eq)
dataDecl :: Name -> [Name] -> Dec
dataDecl tyName ctrNames = DataD
    [] -- no context
    tyName
    [] -- no type vars
    ((flip NormalC) [] `map` ctrNames) -- constructor with no params
    [''Show, ''Eq]

-- instance Enum name where
--     fromEnum alternative = 1
--     ...
--     fromEnum 1 = alternative
--     ...
enumInstance :: Name -> [(Name, Integer)] -> Dec
enumInstance tyName constrs = InstanceD
    [] -- no context
    (AppT (ConT ''Enum) (ConT tyName)) -- Enum name
    -- fromEnum ctr = num
    [
      FunD 'fromEnum
           (((\(ctr, code) ->
                Clause [ConP ctr []]
                       (NormalB (LitE (IntegerL code)))
                       []
             ) `map` constrs)
               ++ [Clause [WildP]
                          (NormalB $ AppE
                              (VarE 'error)
                              (LitE $ StringL $ (nameBase tyName) ++
                                  "fromEnum: bad argument"))
                          []
                  ]
           ),

      -- toEnum num = ctr
      FunD 'toEnum
           (((\(ctr, code) ->
                Clause [LitP (IntegerL code)]
                       (NormalB (ConE ctr))
                       []
             ) `map` constrs)
               ++ [Clause [WildP]
                          (NormalB $ AppE
                              (VarE 'error)
                              -- (LitE $ StringL "toEnum: bad argument"))
                              (LitE $ StringL $ (nameBase tyName) ++
                                  "fromEnum: bad argument"))
                          []
                  ]
           )
    ]


makeEnum :: String -> [(String, Integer)] -> Q [Dec]
makeEnum name alternatives = do
    let tyName = mkName name
        alternatives' = first mkName `map` alternatives
        ctrNames = fst `map` alternatives'
    return [dataDecl tyName ctrNames, enumInstance tyName alternatives']
