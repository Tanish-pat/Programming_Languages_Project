module InventoryRoutes (getAllInventory, getByProductId, updateInventory, checkAvailability) where

import Data.Text (Text, (<>), intercalate)
import qualified Data.Text as T

basePath :: Text
basePath = "/api/inventory"

path :: [Text] -> Text
path segments = basePath <> "/" <> intercalate "/" segments

query :: [(Text, Text)] -> Text
query [] = ""
query ps = "?" <> intercalate "&" [k <> "=" <> v | (k, v) <- ps]

getAllInventory :: Text
getAllInventory = path ["getAll"]

getByProductId :: Text -> Text
getByProductId productId = path ["getByProduct", productId]

updateInventory :: Text -> Text
updateInventory productId = path ["update", productId]

checkAvailability :: Text -> Text
checkAvailability location = path ["availability"] <> query [("location", location)]

