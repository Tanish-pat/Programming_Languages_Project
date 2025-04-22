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