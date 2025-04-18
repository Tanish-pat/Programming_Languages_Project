module UserRoutes (getAllUsers, getById, createNew, updateById, deleteById) where

import Data.Text (Text, (<>))

getAllUsers :: Text
getAllUsers = "/user/getAll"

getById :: Text -> Text
getById x = "/user/getById/" <> x

createNew :: Text
createNew = "/user/create"

updateById :: Text -> Text
updateById x = "/user/update/" <> x

deleteById :: Text -> Text
deleteById x = "/user/delete/" <> x

