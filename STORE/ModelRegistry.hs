{-# LANGUAGE TemplateHaskell #-}

module ModelRegistry (models) where

import Language.Haskell.TH.Syntax (Name)
import Data.Typeable (Typeable)

models :: [(String, [(String, Name)])]
models =
  [ ("User",    [("id", ''Int), ("name", ''String)])
  , ("Product", [("sku", ''String), ("price", ''Double)])
  , ("Order",   [("orderId", ''Int), ("userId", ''Int), ("total", ''Double)])
  ]
