module AddressRoutes (getAllAddresses, getByAddressId, createAddress, updateAddress, deleteAddress, getByUserId) where

import Data.Text (Text, (<>), intercalate)
import qualified Data.Text as T

basePath :: Text
basePath = "/api/address"

path :: [Text] -> Text
path segments = basePath <> "/" <> intercalate "/" segments

query :: [(Text, Text)] -> Text
query [] = ""
query ps = "?" <> intercalate "&" [k <> "=" <> v | (k, v) <- ps]

getAllAddresses :: Text
getAllAddresses = path ["getAll"]

getByAddressId :: Text -> Text
getByAddressId addressId = path ["getById", addressId]

createAddress :: Text
createAddress = path ["create"]

updateAddress :: Text -> Text
updateAddress addressId = path ["update", addressId]

deleteAddress :: Text -> Text
deleteAddress addressId = path ["delete", addressId]

getByUserId :: Text -> Text
getByUserId userId = path ["byUser", userId]

