{-# LANGUAGE DeriveGeneric #-}

module Review where

import Data.Typeable
import Prelude hiding (id)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

data Review = Review {
  reviewReviewId :: Int,
  reviewUserId :: Int,
  reviewProductId :: Text,
  reviewRating :: Int,
  reviewComment :: Text
} deriving (Show, Eq, Typeable, Generic)

instance ToJSON Review
instance FromJSON Review
