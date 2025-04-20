{-# LANGUAGE DeriveGeneric #-}

module Payment where

import Data.Typeable
import Prelude hiding (id)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

data Payment = Payment {
  paymentPaymentId :: Int,
  paymentOrderId :: Int,
  paymentAmount :: Double,
  paymentMethod :: Text,
  paymentStatus :: Text
} deriving (Show, Eq, Typeable, Generic)

instance ToJSON Payment
instance FromJSON Payment
