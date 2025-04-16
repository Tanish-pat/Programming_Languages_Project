module Product where

import Data.Typeable
import Prelude hiding (id)

data Product = Product {
  productSku :: String,
  productName :: String,
  productDescription :: String,
  productPrice :: Double,
  productTags :: String
} deriving (Show, Eq, Typeable)
