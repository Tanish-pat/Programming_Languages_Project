module InventoryRoutes (getAllInventory, getByProductId, updateInventory, checkAvailability) where

import Data.Text (Text, (<>), intercalate)
import qualified Data.Text as T
import Network.HTTP.Types.URI (urlEncode)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

basePath :: Text
basePath = "/api/inventory"

path :: [Text] -> Text
path segments = basePath <> "/" <> intercalate "/" segments

query :: [(Text, Text)] -> Text
query [] = ""
query ps = "?" <> intercalate "&" [k <> "=" <> decodeUtf8 (urlEncode (encodeUtf8 v)) | (k, v) <- ps]

getAllInventory :: Text
getAllInventory = path ["getAll"]

getByProductId :: Text -> Text
getByProductId productId = path ["getByProduct", productId]

updateInventory :: Text -> Text
updateInventory productId = path ["update", productId]

checkAvailability :: Text -> Text
checkAvailability location = path ["availability"] <> query [("location", location)]

