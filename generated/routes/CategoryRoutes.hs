module CategoryRoutes (getAllCategories, getByCategoryId, createCategory, updateCategory, deleteCategory) where

import Data.Text (Text, (<>), intercalate)
import qualified Data.Text as T
import Network.HTTP.Types.URI (urlEncode)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

basePath :: Text
basePath = "/api/category"

path :: [Text] -> Text
path segments = basePath <> "/" <> intercalate "/" segments

query :: [(Text, Text)] -> Text
query [] = ""
query ps = "?" <> intercalate "&" [k <> "=" <> decodeUtf8 (urlEncode (encodeUtf8 v)) | (k, v) <- ps]

getAllCategories :: Text
getAllCategories = path ["getAll"]

getByCategoryId :: Text -> Text
getByCategoryId categoryId = path ["getById", categoryId]

createCategory :: Text
createCategory = path ["create"]

updateCategory :: Text -> Text
updateCategory categoryId = path ["update", categoryId]

deleteCategory :: Text -> Text
deleteCategory categoryId = path ["delete", categoryId]

