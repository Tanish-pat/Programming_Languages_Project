{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Cors (cors, simpleCorsResourcePolicy, CorsResourcePolicy(..))
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
        middleware $ cors (const $ Just corsPolicy)
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

-- CORS Policy to allow frontend (localhost:3001) to communicate
corsPolicy :: CorsResourcePolicy
corsPolicy = simpleCorsResourcePolicy
    { corsOrigins = Just (["http://localhost:3001"], True)
    , corsRequestHeaders = ["Content-Type", "Authorization"]
    , corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
    }