module ProductCategory where

import Data.Typeable
import Prelude hiding (id)

data ProductCategory = ProductCategory {
  productCategoryProductId :: String,
  productCategoryCategoryId :: Int
} deriving (Show, Eq, Typeable)
