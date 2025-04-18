module ProductCategoryRoutes (getAllMappings, getByProductAndCat, createMapping, deleteMapping, getByProduct, getByCategory) where

import Data.Text (Text, (<>), intercalate)
import qualified Data.Text as T

basePath :: Text
basePath = "/api/productcategory"

path :: [Text] -> Text
path segments = basePath <> "/" <> intercalate "/" segments

query :: [(Text, Text)] -> Text
query [] = ""
query ps = "?" <> intercalate "&" [k <> "=" <> v | (k, v) <- ps]

getAllMappings :: Text
getAllMappings = path ["getAll"]

getByProductAndCat :: Text -> Text -> Text
getByProductAndCat productId categoryId = path ["get", productId, categoryId]

createMapping :: Text
createMapping = path ["create"]

deleteMapping :: Text -> Text -> Text
deleteMapping productId categoryId = path ["delete", productId, categoryId]

getByProduct :: Text -> Text
getByProduct productId = path ["byProduct", productId]

getByCategory :: Text -> Text
getByCategory categoryId = path ["byCategory", categoryId]

