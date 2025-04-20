{-# LANGUAGE DeriveGeneric #-}

module ProductCategory where

import Data.Typeable
import Prelude hiding (id)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

data ProductCategory = ProductCategory {
  productCategoryProductId :: Text,
  productCategoryCategoryId :: Int
} deriving (Show, Eq, Typeable, Generic)

instance ToJSON ProductCategory
instance FromJSON ProductCategory
