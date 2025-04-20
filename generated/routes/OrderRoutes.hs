module OrderRoutes (getAllOrders, getByOrderId, createOrder, updateOrder, deleteOrder, getOrdersByUser) where

import Data.Text (Text, (<>), intercalate)
import qualified Data.Text as T
import Network.HTTP.Types.URI (urlEncode)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

basePath :: Text
basePath = "/api/order"

path :: [Text] -> Text
path segments = basePath <> "/" <> intercalate "/" segments

query :: [(Text, Text)] -> Text
query [] = ""
query ps = "?" <> intercalate "&" [k <> "=" <> decodeUtf8 (urlEncode (encodeUtf8 v)) | (k, v) <- ps]

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

