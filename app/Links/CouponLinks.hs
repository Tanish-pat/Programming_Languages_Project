{-# LANGUAGE OverloadedStrings #-}

module Links.CouponLinks (registerRoutes) where

import Web.Scotty
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import Data.Text.Lazy (Text, pack)
import qualified Data.Text.Lazy as TL
import Database.SQLite.Simple
import Database.SQLite.Simple.Types
import Database.SQLite.Simple.FromRow (FromRow(..), field)
import CouponRoutes
import Coupon

sqlDataToText :: SQLData -> Text
sqlDataToText (SQLInteger i) = pack (show i)
sqlDataToText (SQLFloat f)   = pack (show f)
sqlDataToText (SQLText t)    = TL.fromStrict t
sqlDataToText (SQLBlob b)    = pack (show b)
sqlDataToText SQLNull        = "NULL"

-- Check if coupon exists by couponCode
couponExists :: Connection -> Text -> IO Bool
couponExists conn code = do
    res <- query conn "SELECT 1 FROM Coupon WHERE couponCode = ? LIMIT 1" (Only code) :: IO [Only Int]
    return (not (null res))

-- Check for duplicate couponCode
isDuplicateCoupon :: Connection -> Text -> IO Bool
isDuplicateCoupon conn code = do
    res <- query conn "SELECT 1 FROM Coupon WHERE couponCode = ? LIMIT 1" (Only code) :: IO [Only Int]
    return (not $ null res)

registerRoutes :: Connection -> ScottyM ()
registerRoutes conn = do

    -- WORKING GET http://localhost:3000/coupon/getAll
    get "/coupon/getAll" $ do
        rows <- liftIO (query_ conn "SELECT * FROM Coupon" :: IO [[SQLData]])
        json (map (map sqlDataToText) rows)

    -- WORKING GET http://localhost:3000/coupon/get/NEWcustomer10
    get "/coupon/get/:couponCode" $ do
        code <- param "couponCode" :: ActionM Text
        rows <- liftIO (query conn "SELECT * FROM Coupon WHERE couponCode = ?" (Only code) :: IO [[SQLData]])
        json (map (map sqlDataToText) rows)

    -- WORKING POST http://localhost:3000/coupon/create/?couponCode=NEWcustomer1000&discount=30.0&isActive=true
    post "/coupon/create" $ do
        code <- param "couponCode" :: ActionM Text
        disc <- param "discount"   :: ActionM Double
        actv <- param "isActive"   :: ActionM Bool
        isDup <- liftIO $ isDuplicateCoupon conn code
        if isDup
            then json $ object ["status" .= ("error" :: Text), "message" .= ("Coupon code already exists" :: Text)]
            else do
                liftIO $ execute conn "INSERT INTO Coupon (couponCode, discount, isActive) VALUES (?, ?, ?)" (code, disc, actv)
                json (object ["status" .= ("success" :: Text), "message" .= ("Coupon created" :: Text)])

    -- WORKING PUT http://localhost:3000/coupon/update/NEWcustomer1000?couponCode=NEWcustomer1000&discount=30.0&isActive=true
    put "/coupon/update/:couponCode" $ do
        code <- param "couponCode" :: ActionM Text
        disc <- param "discount"   :: ActionM Double
        actv <- param "isActive"   :: ActionM Bool

        -- Check if the coupon exists
        exists <- liftIO $ couponExists conn code
        if not exists
            then json $ object ["status" .= ("error" :: Text), "message" .= ("Coupon not found" :: Text)]
            else do
                liftIO $ execute conn "UPDATE Coupon SET discount = ?, isActive = ? WHERE couponCode = ?" (disc, actv, code)
                json (object ["status" .= ("success" :: Text), "message" .= ("Coupon updated" :: Text)])

    -- WORKING DELETE http://localhost:3000/coupon/delete/NEWcustomer1000
    delete "/coupon/delete/:couponCode" $ do
        code <- param "couponCode" :: ActionM Text

        -- Check if the coupon exists before attempting to delete
        exists <- liftIO $ couponExists conn code
        if not exists
            then json $ object ["status" .= ("error" :: Text), "message" .= ("Coupon not found" :: Text)]
            else do
                liftIO $ execute conn "DELETE FROM Coupon WHERE couponCode = ?" (Only code)
                json (object ["status" .= ("success" :: Text), "message" .= ("Coupon deleted" :: Text)])
