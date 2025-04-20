module ReviewRoutes (getAllReviews, getByReviewId, createReview, updateReview, deleteReview, getReviewsByProduct) where

import Data.Text (Text, (<>), intercalate)
import qualified Data.Text as T

basePath :: Text
basePath = "/api/review"

path :: [Text] -> Text
path segments = basePath <> "/" <> intercalate "/" segments

query :: [(Text, Text)] -> Text
query [] = ""
query ps = "?" <> intercalate "&" [k <> "=" <> v | (k, v) <- ps]

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

