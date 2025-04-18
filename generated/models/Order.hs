module Order where

import Data.Typeable
import Prelude hiding (id)

data Order = Order {
  orderOrderId :: Int,
  orderUserId :: Int,
  orderTotal :: Double,
  orderStatus :: String
} deriving (Show, Eq, Typeable)
