module Order where

import Data.Typeable
import Prelude hiding (id)

data Order = Order {  orderOrderId :: Int,
                      orderUserId :: Int,
                      orderTotal :: Double} deriving (Show,Eq,Typeable)
