{-# LANGUAGE DeriveGeneric #-}

module Address where

import Data.Typeable
import Prelude hiding (id)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

data Address = Address {
  addressAddressId :: Int,
  addressUserId :: Int,
  addressLine1 :: Text,
  addressLine2 :: Text,
  addressCity :: Text,
  addressState :: Text,
  addressZipCode :: Text
} deriving (Show, Eq, Typeable, Generic)

instance ToJSON Address
instance FromJSON Address
