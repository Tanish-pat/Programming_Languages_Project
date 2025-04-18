module ProductRoutes (getAllProducts, getById, createNew, updateById, deleteById) where

import Data.Text (Text, (<>))

getAllProducts :: Text
getAllProducts = "/product/getAll"

getById :: Text -> Text
getById x = "/product/getById/" <> x

createNew :: Text
createNew = "/product/create"

updateById :: Text -> Text
updateById x = "/product/update/" <> x

deleteById :: Text -> Text
deleteById x = "/product/delete/" <> x

