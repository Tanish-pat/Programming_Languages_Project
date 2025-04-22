{-# LANGUAGE OverloadedStrings #-}

module Links.ProductLinks (registerRoutes) where

import Web.Scotty
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import Data.Text.Lazy (Text, pack)
import qualified Data.Text.Lazy as TL
import Database.SQLite.Simple
import Database.SQLite.Simple.Types
import Database.SQLite.Simple.FromRow (FromRow(..), field)
import Data.Monoid ((<>))
import ProductRoutes
import Product

sqlDataToText :: SQLData -> Text
sqlDataToText (SQLInteger i) = pack (show i)
sqlDataToText (SQLFloat   f) = pack (show f)
sqlDataToText (SQLText    t) = TL.fromStrict t
sqlDataToText (SQLBlob    b) = pack (show b)
sqlDataToText SQLNull        = "NULL"

-- Check if product exists by SKU
productExistsBySku :: Connection -> Text -> IO Bool
productExistsBySku conn sku = do
    res <- query conn "SELECT 1 FROM Product WHERE sku = ? LIMIT 1" (Only sku) :: IO [Only Int]
    return (not (null res))


registerRoutes :: Connection -> ScottyM ()
registerRoutes conn = do

    -- WORKING GET http://localhost:3000/product/getAll
    get "/product/getAll" $ do
        rows <- liftIO (query_ conn "SELECT * FROM Product" :: IO [[SQLData]])
        json (map (map sqlDataToText) rows)

    -- WORKING GET http://localhost:3000/product/getBySku/SKU001
    get "/product/getBySku/:sku" $ do
        sku <- param "sku" :: ActionM Text
        rows <- liftIO (query conn
            "SELECT * FROM Product WHERE sku = ?" (Only sku) :: IO [[SQLData]])
        json (map (map sqlDataToText) rows)

    -- WORKING POST http://localhost:3000/product/create?sku=SKU006&name=Paani&description=mehnga paani&price=10000&tags=beverage
    post "/product/create" $ do
        sku         <- param "sku"         :: ActionM Text
        name        <- param "name"        :: ActionM Text
        description <- param "description" :: ActionM Text
        price       <- param "price"       :: ActionM Double
        tags        <- param "tags"        :: ActionM Text
        exists <- liftIO $ productExistsBySku conn sku
        if exists
            then json $ object ["status" .= ("error" :: Text), "message" .= ("Product with this SKU already exists" :: Text)]
            else do
                liftIO $ execute conn
                    "INSERT INTO Product (sku, name, description, price, tags) VALUES (?, ?, ?, ?, ?)"
                    (sku, name, description, price, tags)
                newRow <- liftIO (query conn
                    "SELECT * FROM Product WHERE sku = ?" (Only sku) :: IO [[SQLData]])
                json (map (map sqlDataToText) newRow)

    -- WORKING PUT http://localhost:3000/product/update/SKU006?sku=SKU006&name=Paani&description=mehnga paani&price=10000&tags=beverage
    put "/product/update/:sku" $ do
        sku         <- param "sku"         :: ActionM Text
        name        <- param "name"        :: ActionM Text
        description <- param "description" :: ActionM Text
        price       <- param "price"       :: ActionM Double
        tags        <- param "tags"        :: ActionM Text
        liftIO $ execute conn
            "UPDATE Product SET name = ?, description = ?, price = ?, tags = ? WHERE sku = ?"
            (name, description, price, tags, sku)
        updated <- liftIO (query conn
            "SELECT * FROM Product WHERE sku = ?" (Only sku) :: IO [[SQLData]])
        json (map (map sqlDataToText) updated)

    -- WORKING DELETE http://localhost:3000/product/delete/SKU006
    delete "/product/delete/:sku" $ do
        sku <- param "sku" :: ActionM Text
        exists <- liftIO $ productExistsBySku conn sku
        if not exists
            then json $ object ["status" .= ("error" :: Text), "message" .= ("Product not found" :: Text)]
            else do
                liftIO $ execute conn
                    "DELETE FROM Product WHERE sku = ?" (Only sku)
                json $ object ["status" .= ("success" :: Text), "message" .= ("Product deleted" :: Text)]

    -- WORKING GET http://localhost:3000/product/price/10/1000
    get "/product/price/:min/:max" $ do
        minP <- param "min" :: ActionM Double
        maxP <- param "max" :: ActionM Double
        rows <- liftIO (query conn
            "SELECT * FROM Product WHERE price BETWEEN ? AND ?"
            (minP, maxP) :: IO [[SQLData]])
        json (map (map sqlDataToText) rows)

    -- WORKING GET http://localhost:3000/product/withTag/beverage
    get "/product/withTag/:tag" $ do
        tag <- param "tag" :: ActionM Text
        rows <- liftIO (query conn
            "SELECT * FROM Product WHERE tags LIKE ?" (Only $ "%" <> tag <> "%") :: IO [[SQLData]])
        json (map (map sqlDataToText) rows)

        