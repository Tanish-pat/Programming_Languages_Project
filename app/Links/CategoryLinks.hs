{-# LANGUAGE OverloadedStrings #-}

module Links.CategoryLinks (registerRoutes) where

import Web.Scotty
import Control.Monad.IO.Class (liftIO)
import Data.Text.Lazy (Text, pack)
import qualified Data.Text.Lazy as TL
import Database.SQLite.Simple
import Database.SQLite.Simple.Types
import CategoryRoutes
import Category

sqlDataToText :: SQLData -> Text
sqlDataToText (SQLInteger i) = pack (show i)
sqlDataToText (SQLFloat f)   = pack (show f)
sqlDataToText (SQLText t)    = TL.fromStrict t
sqlDataToText (SQLBlob b)    = pack (show b)
sqlDataToText SQLNull        = "NULL"

registerRoutes :: Connection -> ScottyM ()
registerRoutes conn = do

    -- WORKING GET http://localhost:3000/category/getAll
    get "/category/getAll" $ do
        rows <- liftIO (query_ conn "SELECT * FROM Category" :: IO [[SQLData]])
        json (map (map sqlDataToText) rows)

    -- WORKING GET http://localhost:3000/category/get/1
    get "/category/get/:categoryId" $ do
        cid <- param "categoryId" :: ActionM Int
        rows <- liftIO (query conn "SELECT * FROM Category WHERE categoryId = ?" (Only cid) :: IO [[SQLData]])
        json (map (map sqlDataToText) rows)

    -- WORKING POST http://localhost:3000/category/create?name=Dinner&description=Fully Packed Dinner
    post "/category/create" $ do
        name <- param "name"        :: ActionM Text
        desc <- param "description" :: ActionM Text
        liftIO $ execute conn "INSERT INTO Category (name, description) VALUES (?, ?)" (name, desc)
        json (pack "Category created")

    -- WORKING PUT http://localhost:3000/category/update/6?categoryId=6&name=Dinner&description=Fully Packed Dinner
    put "/category/update/:categoryId" $ do
        cid  <- param "categoryId"  :: ActionM Int
        name <- param "name"        :: ActionM Text
        desc <- param "description" :: ActionM Text
        liftIO $ execute conn "UPDATE Category SET name = ?, description = ? WHERE categoryId = ?" (name, desc, cid)
        json (pack "Category updated")

    -- WORKING DELETE http://localhost:3000/category/delete/6
    delete "/category/delete/:categoryId" $ do
        cid <- param "categoryId" :: ActionM Int
        liftIO $ execute conn "DELETE FROM Category WHERE categoryId = ?" (Only cid)
        json (pack "Category deleted")