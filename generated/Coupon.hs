module Coupon where

import Data.Typeable
import Prelude hiding (id)

data Coupon = Coupon {
  couponCouponCode :: String,
  couponDiscount :: Double,
  couponIsActive :: Bool
} deriving (Show, Eq, Typeable)
