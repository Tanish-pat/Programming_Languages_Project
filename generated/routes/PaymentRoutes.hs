module PaymentRoutes (getAllPayments, getByPaymentId, createPayment, updatePayment, deletePayment, getPaymentsByOrder) where

import Data.Text (Text, (<>), intercalate)
import qualified Data.Text as T
import Network.HTTP.Types.URI (urlEncode)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

basePath :: Text
basePath = "/api/payment"

path :: [Text] -> Text
path segments = basePath <> "/" <> intercalate "/" segments

query :: [(Text, Text)] -> Text
query [] = ""
query ps = "?" <> intercalate "&" [k <> "=" <> decodeUtf8 (urlEncode (encodeUtf8 v)) | (k, v) <- ps]

getAllPayments :: Text
getAllPayments = path ["getAll"]

getByPaymentId :: Text -> Text
getByPaymentId paymentId = path ["getById", paymentId]

createPayment :: Text
createPayment = path ["create"]

updatePayment :: Text -> Text
updatePayment paymentId = path ["update", paymentId]

deletePayment :: Text -> Text
deletePayment paymentId = path ["delete", paymentId]

getPaymentsByOrder :: Text -> Text
getPaymentsByOrder orderId = path ["byOrder", orderId]

