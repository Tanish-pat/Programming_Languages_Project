module CategoryRoutes (getAllCategories, getByCategoryId, createCategory, updateCategory, deleteCategory) where

import Data.Text (Text, (<>), intercalate)
import qualified Data.Text as T

basePath :: Text
basePath = "/api/category"

path :: [Text] -> Text
path segments = basePath <> "/" <> intercalate "/" segments

query :: [(Text, Text)] -> Text
query [] = ""
query ps = "?" <> intercalate "&" [k <> "=" <> v | (k, v) <- ps]

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

