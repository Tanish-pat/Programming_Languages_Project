{-# LANGUAGE DeriveGeneric #-}

module Category where

import Data.Typeable
import Prelude hiding (id)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

data Category = Category {
  categoryCategoryId :: Int,
  categoryName :: Text,
  categoryDescription :: Text
} deriving (Show, Eq, Typeable, Generic)

instance ToJSON Category
instance FromJSON Category
