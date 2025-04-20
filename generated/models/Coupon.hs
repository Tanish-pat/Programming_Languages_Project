{-# LANGUAGE DeriveGeneric #-}

module Coupon where

import Data.Typeable
import Prelude hiding (id)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

data Coupon = Coupon {
  couponCouponCode :: Text,
  couponDiscount :: Double,
  couponIsActive :: Bool
} deriving (Show, Eq, Typeable, Generic)

instance ToJSON Coupon
instance FromJSON Coupon
