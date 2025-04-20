module ProductCategoryRoutes (getAllMappings, getByProductAndCat, createMapping, deleteMapping, getByProduct, getByCategory) where

import Data.Text (Text, (<>), intercalate)
import qualified Data.Text as T
import Network.HTTP.Types.URI (urlEncode)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

basePath :: Text
basePath = "/api/productcategory"

path :: [Text] -> Text
path segments = basePath <> "/" <> intercalate "/" segments

query :: [(Text, Text)] -> Text
query [] = ""
query ps = "?" <> intercalate "&" [k <> "=" <> decodeUtf8 (urlEncode (encodeUtf8 v)) | (k, v) <- ps]

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

