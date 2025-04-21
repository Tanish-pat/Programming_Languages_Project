module Address where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

data Address = Address
  { addressId  :: Int
  , customerId :: Int
  , line1      :: String
  , line2      :: String
  , city       :: String
  , state      :: String
  , zipCode    :: String
  } deriving (Show, Generic)

instance ToJSON Address
instance FromJSON Address
