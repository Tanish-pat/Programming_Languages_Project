module Payment where

import Data.Typeable
import Prelude hiding (id)

data Payment = Payment {
  paymentPaymentId :: Int,
  paymentOrderId :: Int,
  paymentAmount :: Double,
  paymentMethod :: String,
  paymentStatus :: String
} deriving (Show, Eq, Typeable)
