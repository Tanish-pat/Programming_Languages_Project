module OrderRoutes (getAllOrders, getByOrderId, createOrder, updateOrder, deleteOrder, getOrdersByUser) where

import Data.Text (Text, (<>), intercalate)
import qualified Data.Text as T

basePath :: Text
basePath = "/api/order"

path :: [Text] -> Text
path segments = basePath <> "/" <> intercalate "/" segments

query :: [(Text, Text)] -> Text
query [] = ""
query ps = "?" <> intercalate "&" [k <> "=" <> v | (k, v) <- ps]

getAllOrders :: Text
getAllOrders = path ["getAll"]

getByOrderId :: Text -> Text
getByOrderId orderId = path ["getById", orderId]

createOrder :: Text
createOrder = path ["create"]

updateOrder :: Text -> Text
updateOrder orderId = path ["update", orderId]

deleteOrder :: Text -> Text
deleteOrder orderId = path ["delete", orderId]

getOrdersByUser :: Text -> Text
getOrdersByUser userId = path ["byUser", userId]

