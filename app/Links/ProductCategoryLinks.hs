{-# LANGUAGE OverloadedStrings #-}

module Links.ProductCategoryLinks (registerRoutes) where

import Web.Scotty
import Control.Monad.IO.Class (liftIO)
import Data.Text.Lazy (Text, pack)
import qualified Data.Text.Lazy as TL
import Database.SQLite.Simple
import Database.SQLite.Simple.Types
import ProductCategoryRoutes
import ProductCategory

sqlDataToText :: SQLData -> Text
sqlDataToText (SQLInteger i) = pack (show i)
sqlDataToText (SQLFloat f)   = pack (show f)
sqlDataToText (SQLText t)    = TL.fromStrict t
sqlDataToText (SQLBlob b)    = pack (show b)
sqlDataToText SQLNull        = "NULL"

registerRoutes :: Connection -> ScottyM ()
registerRoutes conn = do

    -- WORKING GET http://localhost:3000/product-category/getAll
    get "/product-category/getAll" $ do
        rows <- liftIO (query_ conn "SELECT * FROM ProductCategory" :: IO [[SQLData]])
        json (map (map sqlDataToText) rows)

    -- WORKING POST http://localhost:3000/product-category/assign?productId=SKU001&categoryId=6
    post "/product-category/assign" $ do -- categoryId must exist, productId na ho tab bhi ok hai
        pid <- param "productId"  :: ActionM Text
        cid <- param "categoryId" :: ActionM Int
        liftIO $ execute conn "INSERT INTO ProductCategory (productId, categoryId) VALUES (?, ?)" (pid, cid)
        json (pack "Product assigned to category")

    -- WORKING DELETE http://localhost:3000/product-category/unassign/?productId=SKU001&categoryId=6
    delete "/product-category/unassign" $ do
        pid <- param "productId"  :: ActionM Text
        cid <- param "categoryId" :: ActionM Int
        liftIO $ execute conn "DELETE FROM ProductCategory WHERE productId = ? AND categoryId = ?" (pid, cid)
        json (pack "Product unassigned from category")

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