{-# LANGUAGE DeriveGeneric #-}

module Order where

import Data.Typeable
import Prelude hiding (id)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

data Order = Order {
  orderOrderId :: Int,
  orderUserId :: Int,
  orderTotal :: Double,
  orderStatus :: Text
} deriving (Show, Eq, Typeable, Generic)

instance ToJSON Order
instance FromJSON Order
