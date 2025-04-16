module User where

import Data.Typeable
import Prelude hiding (id)

data User = User {
  userId :: Int,
  userName :: String,
  userEmail :: String,
  userAge :: Int,
  userIsActive :: Bool
} deriving (Show, Eq, Typeable)
