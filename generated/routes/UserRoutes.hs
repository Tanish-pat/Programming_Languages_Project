module UserRoutes (getAllUsers, getById, createUser, updateUser, deleteUser, searchUsers) where

import Data.Text (Text, (<>), intercalate)
import qualified Data.Text as T

basePath :: Text
basePath = "/api/user"

path :: [Text] -> Text
path segments = basePath <> "/" <> intercalate "/" segments

query :: [(Text, Text)] -> Text
query [] = ""
query ps = "?" <> intercalate "&" [k <> "=" <> v | (k, v) <- ps]

getAllUsers :: Text
getAllUsers = path ["getAll"]

getById :: Text -> Text
getById id = path ["getById", id]

createUser :: Text
createUser = path ["create"]

updateUser :: Text -> Text
updateUser id = path ["update", id]

deleteUser :: Text -> Text
deleteUser id = path ["delete", id]

searchUsers :: Text -> Text -> Text
searchUsers name age = path ["search"] <> query [("name", name), ("age", age)]

