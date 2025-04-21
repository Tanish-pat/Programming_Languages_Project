{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Database.SQLite.Simple (open, Connection)

import qualified Links.CustomerLinks as Customer
import qualified Links.ProductLinks as Product
import qualified Links.ReviewLinks as Review
import qualified Links.AddressLinks as Address
import qualified Links.InventoryLinks as Inventory
import qualified Links.PaymentLinks as Payment
import qualified Links.CouponLinks as Coupon
import qualified Links.CategoryLinks as Category
import qualified Links.ProductCategoryLinks as ProductCategory

dbPath :: String
dbPath = "database/INVENTORY.db"

main :: IO ()
main = do
    conn <- open dbPath
    putStrLn "🚀 Starting server on http://localhost:3000 ..."
    scotty 3000 $ do
        middleware logStdoutDev
        get "/" $ text "WELCOME TO THE INVENTORY MANAGEMENT SYSTEM"
        Customer.registerRoutes conn
        Product.registerRoutes conn
        Review.registerRoutes conn
        Address.registerRoutes conn
        Inventory.registerRoutes conn
        Payment.registerRoutes conn
        Coupon.registerRoutes conn
        Category.registerRoutes conn
        ProductCategory.registerRoutes conn





























-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE ScopedTypeVariables #-}

-- module Main where

-- import Web.Scotty
-- import Control.Monad.IO.Class (liftIO)
-- import Network.Wai.Middleware.RequestLogger (logStdoutDev)
-- import Data.Text.Lazy (Text, pack)
-- import qualified Data.Text.Lazy as TL
-- import qualified Data.Text as T
-- import Database.SQLite.Simple (open, query_, query, SQLData(..), Connection, Only(..))

-- sqlDataToText :: SQLData -> Text
-- sqlDataToText (SQLInteger i) = pack (show i)
-- sqlDataToText (SQLFloat f)   = pack (show f)
-- sqlDataToText (SQLText t)    = TL.fromStrict t
-- sqlDataToText (SQLBlob b)    = pack (show b)
-- sqlDataToText SQLNull        = "NULL"

-- -- SQLite database path
-- dbPath :: String
-- dbPath = "database/INVENTORY.db"

-- main :: IO ()
-- main = do
--     conn <- open dbPath
--     putStrLn "🚀 Starting server on http://localhost:3000 ..."
--     scotty 3000 $ do
--         middleware logStdoutDev

--         get "/" $ do
--             text "WELCOME TO THE INVENTORY MANAGEMENT SYSTEM"

--         -- CUSTOMER ROUTES
--         get "/customer/getAll" $ do
--             rows <- liftIO (query_ conn "SELECT * FROM Customer" :: IO [[SQLData]])
--             let textRows = map (map sqlDataToText) rows
--             json textRows

--         get "/customer/getById/:id" $ do
--             id <- param "id"
--             rows <- liftIO (query conn "SELECT * FROM Customer WHERE id = ?" (Only (read id :: Int)) :: IO [[SQLData]])
--             json (map (map sqlDataToText) rows)

--         get "/product/getAll" $ do
--             rows <- liftIO (query_ conn "SELECT * FROM Product" :: IO [[SQLData]])
--             let textRows = map (map sqlDataToText) rows
--             json textRows

--         get "/product/getBySku/:sku" $ do
--             sku <- param "sku"
--             rows <- liftIO (query conn "SELECT * FROM Product WHERE sku = ?" (Only (TL.toStrict sku)) :: IO [[SQLData]])
--             json (map (map sqlDataToText) rows)

--         get "/review/getAll" $ do
--             rows <- liftIO (query_ conn "SELECT * FROM Review" :: IO [[SQLData]])
--             json (map (map sqlDataToText) rows)

--         get "/review/getById/:reviewId" $ do
--             reviewId <- param "reviewId"
--             rows <- liftIO (query conn "SELECT * FROM Review WHERE reviewId = ?" (Only (read reviewId :: Int)) :: IO [[SQLData]])
--             json (map (map sqlDataToText) rows)

--         get "/review/product/:productId" $ do
--             productId <- param "productId"
--             rows <- liftIO (query conn "SELECT * FROM Review WHERE productId = ?" (Only (read productId :: Int)) :: IO [[SQLData]])
--             json (map (map sqlDataToText) rows)

--         get "/address/getAll" $ do
--             rows <- liftIO (query_ conn "SELECT * FROM Address" :: IO [[SQLData]])
--             json (map (map sqlDataToText) rows)

--         get "/address/getById/:addressId" $ do
--             addressId <- param "addressId"
--             rows <- liftIO (query conn "SELECT * FROM Address WHERE addressId = ?" (Only (read addressId :: Int)) :: IO [[SQLData]])
--             json (map (map sqlDataToText) rows)

--         get "/address/byCustomer/:customerId" $ do
--             customerId <- param "customerId"
--             rows <- liftIO (query conn "SELECT * FROM Address WHERE customerId = ?" (Only (read customerId :: Int)) :: IO [[SQLData]])
--             json (map (map sqlDataToText) rows)

--         get "/inventory/getAll" $ do
--             rows <- liftIO (query_ conn "SELECT * FROM Inventory" :: IO [[SQLData]])
--             json (map (map sqlDataToText) rows)

--         get "/inventory/getByProduct/:productId" $ do
--             productId <- param "productId"
--             rows <- liftIO (query conn "SELECT * FROM Inventory WHERE productId = ?" (Only (read productId :: Int)) :: IO [[SQLData]])
--             json (map (map sqlDataToText) rows)

--         get "/payment/getAll" $ do
--             rows <- liftIO (query_ conn "SELECT * FROM Payment" :: IO [[SQLData]])
--             json (map (map sqlDataToText) rows)

--         get "/payment/getById/:paymentId" $ do
--             paymentId <- param "paymentId"
--             rows <- liftIO (query conn "SELECT * FROM Payment WHERE paymentId = ?" (Only (read paymentId :: Int)) :: IO [[SQLData]])
--             json (map (map sqlDataToText) rows)

--         get "/coupon/getAll" $ do
--             rows <- liftIO (query_ conn "SELECT * FROM Coupon" :: IO [[SQLData]])
--             json (map (map sqlDataToText) rows)

--         get "/coupon/getByCode/:couponCode" $ do
--             couponCode <- param "couponCode"
--             rows <- liftIO (query conn "SELECT * FROM Coupon WHERE couponCode = ?" (Only (TL.toStrict couponCode)) :: IO [[SQLData]])
--             json (map (map sqlDataToText) rows)

--         get "/category/getAll" $ do
--             rows <- liftIO (query_ conn "SELECT * FROM Category" :: IO [[SQLData]])
--             json (map (map sqlDataToText) rows)

--         get "/category/getById/:categoryId" $ do
--             categoryId <- param "categoryId"
--             rows <- liftIO (query conn "SELECT * FROM Category WHERE categoryId = ?" (Only (read categoryId :: Int)) :: IO [[SQLData]])
--             json (map (map sqlDataToText) rows)

--         get "/productcategory/getAll" $ do
--             rows <- liftIO (query_ conn "SELECT * FROM ProductCategory" :: IO [[SQLData]])
--             json (map (map sqlDataToText) rows)

--         get "/productcategory/byProduct/:productId" $ do
--             productId <- param "productId"
--             rows <- liftIO (query conn "SELECT * FROM ProductCategory WHERE productId = ?" (Only (read productId :: Int)) :: IO [[SQLData]])
--             json (map (map sqlDataToText) rows)

--         get "/productcategory/byCategory/:categoryId" $ do
--             categoryId <- param "categoryId"
--             rows <- liftIO (query conn "SELECT * FROM ProductCategory WHERE categoryId = ?" (Only (read categoryId :: Int)) :: IO [[SQLData]])
--             json (map (map sqlDataToText) rows)
