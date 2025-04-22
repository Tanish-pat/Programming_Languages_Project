{-# LANGUAGE OverloadedStrings #-}

module Links.CouponLinks (registerRoutes) where

import Web.Scotty
import Control.Monad.IO.Class (liftIO)
import Data.Text.Lazy (Text, pack)
import qualified Data.Text.Lazy as TL
import Database.SQLite.Simple
import Database.SQLite.Simple.Types
import CouponRoutes
import Coupon

sqlDataToText :: SQLData -> Text
sqlDataToText (SQLInteger i) = pack (show i)
sqlDataToText (SQLFloat f)   = pack (show f)
sqlDataToText (SQLText t)    = TL.fromStrict t
sqlDataToText (SQLBlob b)    = pack (show b)
sqlDataToText SQLNull        = "NULL"

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
        liftIO $ execute conn "INSERT INTO Coupon (couponCode, discount, isActive) VALUES (?, ?, ?)" (code, disc, actv)
        json (pack "Coupon created")

    -- WORKING PUT http://localhost:3000/coupon/update/6/?couponCode=NEWcustomer1000&discount=30.0&isActive=true
    put "/coupon/update/:couponCode" $ do
        code <- param "couponCode" :: ActionM Text
        disc <- param "discount"   :: ActionM Double
        actv <- param "isActive"   :: ActionM Bool
        liftIO $ execute conn "UPDATE Coupon SET discount = ?, isActive = ? WHERE couponCode = ?" (disc, actv, code)
        json (pack "Coupon updated")

    -- WORKING DELETE http://localhost:3000/coupon/delete/6
    delete "/coupon/delete/:couponCode" $ do
        code <- param "couponCode" :: ActionM Text
        liftIO $ execute conn "DELETE FROM Coupon WHERE couponCode = ?" (Only code)
        json (pack "Coupon deleted")
