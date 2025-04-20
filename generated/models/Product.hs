{-# LANGUAGE DeriveGeneric #-}

module Product where

import Data.Typeable
import Prelude hiding (id)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

data Product = Product {
  productSku :: Text,
  productName :: Text,
  productDescription :: Text,
  productPrice :: Double,
  productTags :: Text
} deriving (Show, Eq, Typeable, Generic)

instance ToJSON Product
instance FromJSON Product
