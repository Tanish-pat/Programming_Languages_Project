module ProductRoutes (getAllProducts, getBySku, createProduct, updateProduct, deleteProduct, searchProducts) where

import Data.Text (Text, (<>), intercalate)
import qualified Data.Text as T

basePath :: Text
basePath = "/api/product"

path :: [Text] -> Text
path segments = basePath <> "/" <> intercalate "/" segments

query :: [(Text, Text)] -> Text
query [] = ""
query ps = "?" <> intercalate "&" [k <> "=" <> v | (k, v) <- ps]

getAllProducts :: Text
getAllProducts = path ["getAll"]

getBySku :: Text -> Text
getBySku sku = path ["getBySku", sku]

createProduct :: Text
createProduct = path ["create"]

updateProduct :: Text -> Text
updateProduct sku = path ["update", sku]

deleteProduct :: Text -> Text
deleteProduct sku = path ["delete", sku]

searchProducts :: Text -> Text -> Text
searchProducts name tags = path ["search"] <> query [("name", name), ("tags", tags)]

