{-# LANGUAGE DeriveGeneric #-}

module User where

import Data.Typeable
import Prelude hiding (id)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

data User = User {
  userId :: Int,
  userName :: Text,
  userEmail :: Text,
  userAge :: Int,
  userIsActive :: Bool
} deriving (Show, Eq, Typeable, Generic)

instance ToJSON User
instance FromJSON User
