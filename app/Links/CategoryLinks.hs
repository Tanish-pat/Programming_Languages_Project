{-# LANGUAGE OverloadedStrings #-}

module Links.CategoryLinks (registerRoutes) where

import Web.Scotty
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import Data.Text.Lazy (Text, pack)
import qualified Data.Text.Lazy as TL
import Database.SQLite.Simple
import Database.SQLite.Simple.Types
import Database.SQLite.Simple.FromRow (FromRow(..), field)
import CategoryRoutes
import Category

sqlDataToText :: SQLData -> Text
sqlDataToText (SQLInteger i) = pack (show i)
sqlDataToText (SQLFloat f)   = pack (show f)
sqlDataToText (SQLText t)    = TL.fromStrict t
sqlDataToText (SQLBlob b)    = pack (show b)
sqlDataToText SQLNull        = "NULL"

-- Check if category exists by id
categoryExists :: Connection -> Int -> IO Bool
categoryExists conn cid = do
    res <- query conn "SELECT 1 FROM Category WHERE categoryId = ? LIMIT 1" (Only cid) :: IO [Only Int]
    return (not $ null res)

-- Check for duplicate name
isDuplicateCategory :: Connection -> Text -> IO Bool
isDuplicateCategory conn name = do
    res <- query conn "SELECT 1 FROM Category WHERE name = ? LIMIT 1" (Only name) :: IO [Only Int]
    return (not $ null res)

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
        isDup <- liftIO $ isDuplicateCategory conn name
        if isDup
            then json $ object ["status" .= ("error" :: Text), "message" .= ("Category name already exists" :: Text)]
            else do
                liftIO $ execute conn "INSERT INTO Category (name, description) VALUES (?, ?)" (name, desc)
                json $ object ["status" .= ("success" :: Text), "message" .= ("Category created" :: Text)]

    -- WORKING PUT http://localhost:3000/category/update/6?categoryId=6&name=Dinner&description=Fully Packed Dinner
    put "/category/update/:categoryId" $ do
        cid  <- param "categoryId"  :: ActionM Int
        name <- param "name"        :: ActionM Text
        desc <- param "description" :: ActionM Text

        exists <- liftIO $ categoryExists conn cid
        if not exists
            then json $ object ["status" .= ("error" :: Text), "message" .= ("Category not found" :: Text)]
            else do
                isDup <- liftIO $ query conn
                            "SELECT 1 FROM Category WHERE name = ? AND categoryId != ? LIMIT 1"
                            (name, cid) :: ActionM [Only Int]
                if not (null isDup)
                    then json $ object ["status" .= ("error" :: Text), "message" .= ("Another category with the same name exists" :: Text)]
                    else do
                        liftIO $ execute conn
                            "UPDATE Category SET name = ?, description = ? WHERE categoryId = ?"
                            (name, desc, cid)
                        json $ object ["status" .= ("success" :: Text), "message" .= ("Category updated" :: Text)]

    -- WORKING DELETE http://localhost:3000/category/delete/6
    delete "/category/delete/:categoryId" $ do
        cid <- param "categoryId" :: ActionM Int
        exists <- liftIO $ categoryExists conn cid
        if not exists
            then json $ object ["status" .= ("error" :: Text), "message" .= ("Category not found" :: Text)]
            else do
                liftIO $ execute conn "DELETE FROM Category WHERE categoryId = ?" (Only cid)
                json $ object ["status" .= ("success" :: Text), "message" .= ("Category deleted" :: Text)]
