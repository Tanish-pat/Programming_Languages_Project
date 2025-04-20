{-# LANGUAGE DeriveGeneric #-}

module Inventory where

import Data.Typeable
import Prelude hiding (id)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

data Inventory = Inventory {
  inventoryProductId :: Text,
  inventoryQuantity :: Int,
  inventoryLocation :: Text
} deriving (Show, Eq, Typeable, Generic)

instance ToJSON Inventory
instance FromJSON Inventory
