module ReviewRoutes (getAllReviews, getByReviewId, createReview, updateReview, deleteReview, getReviewsByProduct) where

import Data.Text (Text, (<>), intercalate)
import qualified Data.Text as T
import Network.HTTP.Types.URI (urlEncode)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

basePath :: Text
basePath = "/api/review"

path :: [Text] -> Text
path segments = basePath <> "/" <> intercalate "/" segments

query :: [(Text, Text)] -> Text
query [] = ""
query ps = "?" <> intercalate "&" [k <> "=" <> decodeUtf8 (urlEncode (encodeUtf8 v)) | (k, v) <- ps]

getAllReviews :: Text
getAllReviews = path ["getAll"]

getByReviewId :: Text -> Text
getByReviewId reviewId = path ["getById", reviewId]

createReview :: Text
createReview = path ["create"]

updateReview :: Text -> Text
updateReview reviewId = path ["update", reviewId]

deleteReview :: Text -> Text
deleteReview reviewId = path ["delete", reviewId]

getReviewsByProduct :: Text -> Text -> Text
getReviewsByProduct productId minRating = path ["product", productId] <> query [("minRating", minRating)]

