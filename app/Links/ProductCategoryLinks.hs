{-# LANGUAGE OverloadedStrings #-}

module Links.ProductCategoryLinks (registerRoutes) where

import Web.Scotty
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import Data.Text.Lazy (Text, pack)
import qualified Data.Text.Lazy as TL
import Database.SQLite.Simple
import Database.SQLite.Simple.Types
import Database.SQLite.Simple.FromRow (FromRow(..), field)
import ProductCategoryRoutes
import ProductCategory

sqlDataToText :: SQLData -> Text
sqlDataToText (SQLInteger i) = pack (show i)
sqlDataToText (SQLFloat f)   = pack (show f)
sqlDataToText (SQLText t)    = TL.fromStrict t
sqlDataToText (SQLBlob b)    = pack (show b)
sqlDataToText SQLNull        = "NULL"

-- Check if category exists
categoryExists :: Connection -> Int -> IO Bool
categoryExists conn cid = do
    res <- query conn "SELECT 1 FROM Category WHERE categoryId = ? LIMIT 1" (Only cid) :: IO [Only Int]
    return (not (null res))

-- Check if mapping already exists
mappingExists :: Connection -> Text -> Int -> IO Bool
mappingExists conn pid cid = do
    res <- query conn "SELECT 1 FROM ProductCategory WHERE productId = ? AND categoryId = ? LIMIT 1" (pid, cid) :: IO [Only Int]
    return (not (null res))

registerRoutes :: Connection -> ScottyM ()
registerRoutes conn = do

    -- WORKING GET http://localhost:3000/product-category/getAll
    get "/product-category/getAll" $ do
        rows <- liftIO (query_ conn "SELECT * FROM ProductCategory" :: IO [[SQLData]])
        json (map (map sqlDataToText) rows)

    -- WORKING POST http://localhost:3000/product-category/assign?productId=SKU001&categoryId=5
    post "/product-category/assign" $ do
        pid <- param "productId"  :: ActionM Text
        cid <- param "categoryId" :: ActionM Int

        -- Check if the category exists
        catExists <- liftIO $ categoryExists conn cid
        if not catExists
            then json $ object ["status" .= ("error" :: Text), "message" .= ("Category not found" :: Text)]
            else do
                -- Check if the product is already assigned to the category
                assigned <- liftIO $ mappingExists conn pid cid
                if assigned
                    then json $ object ["status" .= ("error" :: Text), "message" .= ("Product is already assigned to this category" :: Text)]
                    else do
                        -- Assign the product to the category
                        liftIO $ execute conn "INSERT INTO ProductCategory (productId, categoryId) VALUES (?, ?)" (pid, cid)
                        json $ object ["status" .= ("success" :: Text), "message" .= ("Product assigned to category" :: Text)]

    -- WORKING DELETE http://localhost:3000/product-category/unassign/?productId=SKU001&categoryId=6
    delete "/product-category/unassign" $ do
        pid <- param "productId"  :: ActionM Text
        cid <- param "categoryId" :: ActionM Int

        -- Check if the product is assigned to the category
        assigned <- liftIO $ mappingExists conn pid cid
        if not assigned
            then json $ object ["status" .= ("error" :: Text), "message" .= ("Mapping not found" :: Text)]
            else do
                -- Unassign the product from the category
                liftIO $ execute conn "DELETE FROM ProductCategory WHERE productId = ? AND categoryId = ?" (pid, cid)
                json $ object ["status" .= ("success" :: Text), "message" .= ("Product unassigned from category" :: Text)]

    -- WORKING GET http://localhost:3000/product-category/byProduct/SKU001
    get "/product-category/byProduct/:productId" $ do
        pid <- param "productId" :: ActionM Text
        rows <- liftIO (query conn "SELECT * FROM ProductCategory WHERE productId = ?" (Only pid) :: IO [[SQLData]])
        json (map (map sqlDataToText) rows)

    -- WORKING GET http://localhost:3000/product-category/byCategory/2
    get "/product-category/byCategory/:categoryId" $ do
        cid <- param "categoryId" :: ActionM Int
        rows <- liftIO (query conn "SELECT * FROM ProductCategory WHERE categoryId = ?" (Only cid) :: IO [[SQLData]])
        json (map (map sqlDataToText) rows)