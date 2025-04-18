module Review where

import Data.Typeable
import Prelude hiding (id)

data Review = Review {
  reviewReviewId :: Int,
  reviewUserId :: Int,
  reviewProductId :: String,
  reviewRating :: Int,
  reviewComment :: String
} deriving (Show, Eq, Typeable)
