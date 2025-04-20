module AddressRoutes (getAllAddresses, getByAddressId, createAddress, updateAddress, deleteAddress, getByUserId) where

import Data.Text (Text, (<>), intercalate)
import qualified Data.Text as T
import Network.HTTP.Types.URI (urlEncode)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

basePath :: Text
basePath = "/api/address"

path :: [Text] -> Text
path segments = basePath <> "/" <> intercalate "/" segments

query :: [(Text, Text)] -> Text
query [] = ""
query ps = "?" <> intercalate "&" [k <> "=" <> decodeUtf8 (urlEncode (encodeUtf8 v)) | (k, v) <- ps]

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

