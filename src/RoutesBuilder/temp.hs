module UserRoutes (getAllUsers, getById, createNew, updateById, deleteById) where

import Data.Text (Text, (<>))

getAllUsers :: Text
getAllUsers = "/user/getAll"

getById :: Text -> Text
getById userId = "/user/getById/" <> userId

createNew :: Text
createNew = "/user/create"

updateById :: Text -> Text
updateById userId = "/user/update/" <> userId

deleteById :: Text -> Text
deleteById userId = "/user/delete/" <> userId