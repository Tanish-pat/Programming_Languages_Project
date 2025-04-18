module Category where

import Data.Typeable
import Prelude hiding (id)

data Category = Category {
  categoryCategoryId :: Int,
  categoryName :: String,
  categoryDescription :: String
} deriving (Show, Eq, Typeable)
