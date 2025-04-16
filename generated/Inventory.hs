module Inventory where

import Data.Typeable
import Prelude hiding (id)

data Inventory = Inventory {
  inventoryProductId :: String,
  inventoryQuantity :: Int,
  inventoryLocation :: String
} deriving (Show, Eq, Typeable)
