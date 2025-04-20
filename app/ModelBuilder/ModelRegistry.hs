{-# LANGUAGE TemplateHaskell #-}

module ModelRegistry (models) where

import Language.Haskell.TH.Syntax (Name)
import qualified Data.Text as T
import Data.Typeable (Typeable)

models :: [(String, [(String, Name)])]
models =
  [ ("User",
      [ ("id", ''Int)
      , ("name", ''T.Text)
      , ("email", ''T.Text)
      , ("age", ''Int)
      , ("isActive", ''Bool)
      ])

  , ("Product",
      [ ("sku", ''T.Text)
      , ("name", ''T.Text)
      , ("description", ''T.Text)
      , ("price", ''Double)
      , ("tags", ''T.Text)
      ])

  , ("Order",
      [ ("orderId", ''Int)
      , ("userId", ''Int)
      , ("total", ''Double)
      , ("status", ''T.Text)
      ])

  , ("Review",
      [ ("reviewId", ''Int)
      , ("userId", ''Int)
      , ("productId", ''T.Text)
      , ("rating", ''Int)
      , ("comment", ''T.Text)
      ])

  , ("Address",
      [ ("addressId", ''Int)
      , ("userId", ''Int)
      , ("line1", ''T.Text)
      , ("line2", ''T.Text)
      , ("city", ''T.Text)
      , ("state", ''T.Text)
      , ("zipCode", ''T.Text)
      ])

  , ("Inventory",
      [ ("productId", ''T.Text)
      , ("quantity", ''Int)
      , ("location", ''T.Text)
      ])

  , ("Payment",
      [ ("paymentId", ''Int)
      , ("orderId", ''Int)
      , ("amount", ''Double)
      , ("method", ''T.Text)
      , ("status", ''T.Text)
      ])

  , ("Coupon",
      [ ("couponCode", ''T.Text)
      , ("discount", ''Double)
      , ("isActive", ''Bool)
      ])

  , ("Category",
      [ ("categoryId", ''Int)
      , ("name", ''T.Text)
      , ("description", ''T.Text)
      ])

  , ("ProductCategory",
      [ ("productId", ''T.Text)
      , ("categoryId", ''Int)
      ])
  ]





-- {-# LANGUAGE TemplateHaskell #-}

-- module ModelRegistry (models) where

-- import Language.Haskell.TH.Syntax (Name)
-- import Data.Typeable (Typeable)

-- models :: [(String, [(String, Name)])]
-- models =
--   [ ("User",
--       [ ("id", ''Int)
--       , ("name", ''String)
--       , ("email", ''String)
--       , ("age", ''Int)
--       , ("isActive", ''Bool)
--       ])

--   , ("Product",
--       [ ("sku", ''String)
--       , ("name", ''String)
--       , ("description", ''String)
--       , ("price", ''Double)
--       , ("tags", ''String)
--       ])

--   , ("Order",
--       [ ("orderId", ''Int)
--       , ("userId", ''Int)  -- foreign key
--       , ("total", ''Double)
--       , ("status", ''String)
--       ])

--   , ("Review",
--       [ ("reviewId", ''Int)
--       , ("userId", ''Int)
--       , ("productId", ''String)
--       , ("rating", ''Int)
--       , ("comment", ''String)
--       ])

--   , ("Address",
--       [ ("addressId", ''Int)
--       , ("userId", ''Int)
--       , ("line1", ''String)
--       , ("line2", ''String)
--       , ("city", ''String)
--       , ("state", ''String)
--       , ("zipCode", ''String)
--       ])

--   , ("Inventory",
--       [ ("productId", ''String)
--       , ("quantity", ''Int)
--       , ("location", ''String)
--       ])

--   , ("Payment",
--       [ ("paymentId", ''Int)
--       , ("orderId", ''Int)
--       , ("amount", ''Double)
--       , ("method", ''String)
--       , ("status", ''String)
--       ])

--   , ("Coupon",
--       [ ("couponCode", ''String)
--       , ("discount", ''Double)
--       , ("isActive", ''Bool)
--       ])

--   , ("Category",
--       [ ("categoryId", ''Int)
--       , ("name", ''String)
--       , ("description", ''String)
--       ])

--   , ("ProductCategory",
--       [ ("productId", ''String)
--       , ("categoryId", ''Int)
--       ])
--   ]
