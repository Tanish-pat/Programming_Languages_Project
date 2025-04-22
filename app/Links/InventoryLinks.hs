{-# LANGUAGE OverloadedStrings #-}

module Links.InventoryLinks (registerRoutes) where

import Web.Scotty
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import Data.Text.Lazy (Text, pack)
import qualified Data.Text.Lazy as TL
import Database.SQLite.Simple
import Database.SQLite.Simple.Types
import Database.SQLite.Simple.FromRow (FromRow(..), field)
import InventoryRoutes
import Inventory

sqlDataToText :: SQLData -> Text
sqlDataToText (SQLInteger i) = pack (show i)
sqlDataToText (SQLFloat   f) = pack (show f)
sqlDataToText (SQLText    t) = TL.fromStrict t
sqlDataToText (SQLBlob    b) = pack (show b)
sqlDataToText SQLNull        = "NULL"

-- Utility: Check if inventory item exists by productId
inventoryExists :: Connection -> Text -> IO Bool
inventoryExists conn pid = do
    res <- query conn "SELECT 1 FROM Inventory WHERE productId = ? LIMIT 1" (Only pid) :: IO [Only Int]
    return (not (null res))

registerRoutes :: Connection -> ScottyM ()
registerRoutes conn = do

    -- WORKING GET http://localhost:3000/inventory/getAll
    get "/inventory/getAll" $ do
        rows <- liftIO (query_ conn "SELECT * FROM Inventory" :: IO [[SQLData]])
        json (map (map sqlDataToText) rows)

    -- WORKING GET http://localhost:3000/inventory/get/SKU001
    get "/inventory/get/:productId" $ do
        pid <- param "productId" :: ActionM Text
        rows <- liftIO (query conn "SELECT * FROM Inventory WHERE productId = ?" (Only pid) :: IO [[SQLData]])
        json (map (map sqlDataToText) rows)

    -- WORKING POST http://localhost:3000/inventory/create?productId=SKU006&quantity=100&location=Punjab
    post "/inventory/create" $ do
        pid <- param "productId" :: ActionM Text
        qty <- param "quantity"  :: ActionM Int
        loc <- param "location"  :: ActionM Text

        exists <- liftIO $ inventoryExists conn pid
        if exists
            then json $ object ["status" .= ("error" :: Text), "message" .= ("Inventory item already exists" :: Text)]
            else do
                liftIO $ execute conn "INSERT INTO Inventory (productId, quantity, location) VALUES (?, ?, ?)" (pid, qty, loc)
                json $ object ["status" .= ("success" :: Text), "message" .= ("Inventory item created" :: Text)]

    -- WORKING PUT http://localhost:3000/inventory/update/SKU006?productId=SKU006&quantity=100&location=Punjab
    put "/inventory/update/:productId" $ do
        pid <- param "productId" :: ActionM Text
        qty <- param "quantity"  :: ActionM Int
        loc <- param "location"  :: ActionM Text

        exists <- liftIO $ inventoryExists conn pid
        if not exists
            then json $ object ["status" .= ("error" :: Text), "message" .= ("Inventory item not found" :: Text)]
            else do
                liftIO $ execute conn "UPDATE Inventory SET quantity = ?, location = ? WHERE productId = ?" (qty, loc, pid)
                json $ object ["status" .= ("success" :: Text), "message" .= ("Inventory item updated" :: Text)]

    -- WORKING DELETE http://localhost:3000/inventory/delete/SKU006
    delete "/inventory/delete/:productId" $ do
        pid <- param "productId" :: ActionM Text

        exists <- liftIO $ inventoryExists conn pid
        if not exists
            then json $ object ["status" .= ("error" :: Text), "message" .= ("Inventory item not found" :: Text)]
            else do
                liftIO $ execute conn "DELETE FROM Inventory WHERE productId = ?" (Only pid)
                json $ object ["status" .= ("success" :: Text), "message" .= ("Inventory item deleted" :: Text)]
