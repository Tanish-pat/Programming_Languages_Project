{-# LANGUAGE TemplateHaskell #-}

module THGenerated (makeTransformations) where

import Language.Haskell.TH

-- COMPILE TIME FUNCTION GENERATOR
makeTransformations :: Q [Dec]
makeTransformations = do
    let transformations =
          [ ("reverseString", [| reverse |])
          , ("toUpperCase",   [| map toUpper |])
          , ("addExclaim",    [| (++ "!!!") |])
          ]

    fmap concat $
        mapM
            (\(name, expr) -> do
            d <- funD
                    (mkName name)
                    [clause [varP (mkName "x")] (normalB [| $expr x |]) []]
            return [d]
            )
            transformations