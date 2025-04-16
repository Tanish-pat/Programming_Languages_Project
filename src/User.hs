module User where

import Data.Typeable
import Prelude hiding (id)

data User = User {  userId :: Int,
                    userName :: String} deriving (Show,Eq,Typeable)
