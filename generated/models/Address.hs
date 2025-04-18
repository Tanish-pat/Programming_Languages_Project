module Address where

import Data.Typeable
import Prelude hiding (id)

data Address = Address {
  addressAddressId :: Int,
  addressUserId :: Int,
  addressLine1 :: String,
  addressLine2 :: String,
  addressCity :: String,
  addressState :: String,
  addressZipCode :: String
} deriving (Show, Eq, Typeable)
