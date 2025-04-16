module Product where

import Data.Typeable
import Prelude hiding (id)

data Product = Product {
  productSku :: String,
  productPrice :: Double
} deriving (Show, Eq, Typeable)
