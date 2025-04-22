{-# LANGUAGE OverloadedStrings #-}

module Links.InventoryLinks (registerRoutes) where

import Web.Scotty
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy (Text, pack)
import Database.SQLite.Simple
import Database.SQLite.Simple.Types
import InventoryRoutes
import Inventory

sqlDataToText :: SQLData -> Text
sqlDataToText (SQLInteger i) = pack (show i)
sqlDataToText (SQLFloat f)   = pack (show f)
sqlDataToText (SQLText t)    = TL.fromStrict t
sqlDataToText (SQLBlob b)    = pack (show b)
sqlDataToText SQLNull        = "NULL"

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
        liftIO $ execute conn "INSERT INTO Inventory (productId, quantity, location) VALUES (?, ?, ?)" (pid, qty, loc)
        json (pack $ "Inserted inventory for productId=" ++ TL.unpack pid)

    -- WORKING PUT http://localhost:3000/inventory/update/SKU006?productId=SKU006&quantity=100&location=Punjab
    put "/inventory/update/:productId" $ do
        pid <- param "productId" :: ActionM Text
        qty <- param "quantity"  :: ActionM Int
        loc <- param "location"  :: ActionM Text
        liftIO $ execute conn "UPDATE Inventory SET quantity = ?, location = ? WHERE productId = ?" (qty, loc, pid)
        json (pack $ "Updated inventory for productId=" ++ TL.unpack pid)

    -- WORKING DELETE http://localhost:3000/inventory/delete/SKU006
    delete "/inventory/delete/:productId" $ do
        pid <- param "productId" :: ActionM Text
        liftIO $ execute conn "DELETE FROM Inventory WHERE productId = ?" (Only pid)
        json (pack $ "Deleted inventory for productId=" ++ TL.unpack pid)
