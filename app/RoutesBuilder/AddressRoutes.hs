module AddressRoutes where

import Data.Text (Text)

getAllAddresses :: Text
getAllAddresses = "/address/getAll"

getByAddressId :: Text -> Text
getByAddressId param = "/address/getById/" <> param

getByCustomerId :: Text -> Text
getByCustomerId param = "/address/byCustomer/" <> param
