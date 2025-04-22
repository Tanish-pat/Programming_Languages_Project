{-# LANGUAGE OverloadedStrings #-}

module Links.ReviewLinks (registerRoutes) where

import Web.Scotty
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import Data.Text.Lazy (Text, pack)
import qualified Data.Text.Lazy as TL
import Database.SQLite.Simple
import Database.SQLite.Simple.Types
import Database.SQLite.Simple.FromRow (FromRow(..), field)
import Data.Monoid ((<>))
import ReviewRoutes
import Review

sqlDataToText :: SQLData -> Text
sqlDataToText (SQLInteger i) = pack (show i)
sqlDataToText (SQLFloat   f) = pack (show f)
sqlDataToText (SQLText    t) = TL.fromStrict t
sqlDataToText (SQLBlob    b) = pack (show b)
sqlDataToText SQLNull        = "NULL"

-- Check if review exists by reviewId
reviewExistsById :: Connection -> Int -> IO Bool
reviewExistsById conn reviewId = do
    res <- query conn "SELECT 1 FROM Review WHERE reviewId = ? LIMIT 1" (Only reviewId) :: IO [Only Int]
    return (not (null res))

-- Check if review exists for customerId + productId
reviewExistsByCustomerAndProduct :: Connection -> Int -> Text -> IO Bool
reviewExistsByCustomerAndProduct conn customerId productId = do
    res <- query conn
        "SELECT 1 FROM Review WHERE customerId = ? AND productId = ? LIMIT 1"
        (customerId, productId) :: IO [Only Int]
    return (not (null res))

registerRoutes :: Connection -> ScottyM ()
registerRoutes conn = do

    -- WORKING GET http://localhost:3000/review/getAll
    get "/review/getAll" $ do
        rows <- liftIO (query_ conn "SELECT * FROM Review" :: IO [[SQLData]])
        json (map (map sqlDataToText) rows)

    -- WORKING GET http://localhost:3000/review/getById/1
    get "/review/getById/:reviewId" $ do
        reviewId <- param "reviewId" :: ActionM Int
        rows <- liftIO (query conn "SELECT * FROM Review WHERE reviewId = ?" (Only reviewId) :: IO [[SQLData]])
        json (map (map sqlDataToText) rows)

    -- WORKING GET http://localhost:3000/review/product/SKU001
    get "/review/product/:productId" $ do
        productId <- param "productId" :: ActionM Text
        rows <- liftIO (query conn "SELECT * FROM Review WHERE productId = ?" (Only productId) :: IO [[SQLData]])
        json (map (map sqlDataToText) rows)

    -- WORKING GET http://localhost:3000/review/customer/1
    get "/review/customer/:customerId" $ do
        customerId <- param "customerId" :: ActionM Int
        rows <- liftIO (query conn "SELECT * FROM Review WHERE customerId = ?" (Only customerId) :: IO [[SQLData]])
        json (map (map sqlDataToText) rows)

    -- WORKING POST http://localhost:3000/review/create?customerId=1&productId=SKU005&rating=4&comment=meh hi tha, maza nhi aaya
    post "/review/create" $ do
        customerId <- param "customerId" :: ActionM Int
        productId  <- param "productId"  :: ActionM Text
        rating     <- param "rating"     :: ActionM Int
        comment    <- param "comment"    :: ActionM Text

        exists <- liftIO $ reviewExistsByCustomerAndProduct conn customerId productId
        if exists
            then json $ object ["status" .= ("error" :: Text), "message" .= ("Duplicate review not allowed for same customer and product" :: Text)]
            else do
                liftIO $ execute conn
                    "INSERT INTO Review (customerId, productId, rating, comment) VALUES (?, ?, ?, ?)"
                    (customerId, productId, rating, comment)
                newRow <- liftIO (query_ conn
                    "SELECT * FROM Review ORDER BY reviewId DESC LIMIT 1" :: IO [[SQLData]])
                json (map (map sqlDataToText) newRow)

    -- WORKING PUT http://localhost:3000/review/update/2?reviewId=2&customerId=2&productId=SKU001&rating=5&comment=acha tha
    put "/review/update/:reviewId" $ do
        reviewId   <- param "reviewId"   :: ActionM Int
        customerId <- param "customerId" :: ActionM Int
        productId  <- param "productId"  :: ActionM Text
        rating     <- param "rating"     :: ActionM Int
        comment    <- param "comment"    :: ActionM Text

        exists <- liftIO $ reviewExistsById conn reviewId
        if not exists
            then json $ object ["status" .= ("error" :: Text), "message" .= ("Review not found" :: Text)]
            else do
                liftIO $ execute conn
                    "UPDATE Review SET customerId = ?, productId = ?, rating = ?, comment = ? WHERE reviewId = ?"
                    (customerId, productId, rating, comment, reviewId)
                updated <- liftIO (query conn
                    "SELECT * FROM Review WHERE reviewId = ?" (Only reviewId) :: IO [[SQLData]])
                json (map (map sqlDataToText) updated)

    -- WORKING DELETE http://localhost:3000/review/delete/6
    delete "/review/delete/:reviewId" $ do
        reviewId <- param "reviewId" :: ActionM Int
        exists <- liftIO $ reviewExistsById conn reviewId
        if not exists
            then json $ object ["status" .= ("error" :: Text), "message" .= ("Review not found" :: Text)]
            else do
                liftIO $ execute conn
                    "DELETE FROM Review WHERE reviewId = ?" (Only reviewId)
                json $ object ["status" .= ("success" :: Text), "message" .= ("Review deleted" :: Text)]

    -- WORKING GET http://localhost:3000/review/rating/0/4
    get "/review/rating/:min/:max" $ do
        minR <- param "min" :: ActionM Int
        maxR <- param "max" :: ActionM Int
        rows <- liftIO (query conn
            "SELECT * FROM Review WHERE rating BETWEEN ? AND ?"
            (minR, maxR) :: IO [[SQLData]])
        json (map (map sqlDataToText) rows)

