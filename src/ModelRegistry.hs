{-# LANGUAGE TemplateHaskell #-}

module ModelRegistry (models) where

import Language.Haskell.TH.Syntax (Name)
import Data.Typeable (Typeable)

models :: [(String, [(String, Name)])]
models =
  [ ("User",
      [ ("id", ''Int)
      , ("name", ''String)
      , ("email", ''String)
      , ("age", ''Int)
      , ("isActive", ''Bool)
      ])

  , ("Product",
      [ ("sku", ''String)
      , ("name", ''String)
      , ("description", ''String)
      , ("price", ''Double)
      , ("tags", ''String)
      ])

  , ("Order",
      [ ("orderId", ''Int)
      , ("userId", ''Int)  -- foreign key
      , ("total", ''Double)
      , ("status", ''String)
      ])

  , ("Review",
      [ ("reviewId", ''Int)
      , ("userId", ''Int)
      , ("productId", ''String)
      , ("rating", ''Int)
      , ("comment", ''String)
      ])

  , ("Address",
      [ ("addressId", ''Int)
      , ("userId", ''Int)
      , ("line1", ''String)
      , ("line2", ''String)
      , ("city", ''String)
      , ("state", ''String)
      , ("zipCode", ''String)
      ])

  , ("Inventory",
      [ ("productId", ''String)
      , ("quantity", ''Int)
      , ("location", ''String)
      ])

  , ("Payment",
      [ ("paymentId", ''Int)
      , ("orderId", ''Int)
      , ("amount", ''Double)
      , ("method", ''String)
      , ("status", ''String)
      ])

  , ("Coupon",
      [ ("couponCode", ''String)
      , ("discount", ''Double)
      , ("isActive", ''Bool)
      ])

  , ("Category",
      [ ("categoryId", ''Int)
      , ("name", ''String)
      , ("description", ''String)
      ])

  , ("ProductCategory",
      [ ("productId", ''String)
      , ("categoryId", ''Int)
      ])
  ]
