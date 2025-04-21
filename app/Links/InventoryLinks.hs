{-# LANGUAGE OverloadedStrings #-}

module Links.InventoryLinks (registerRoutes) where

import Web.Scotty
import Control.Monad.IO.Class (liftIO)
import Data.Text.Lazy (Text, pack)
import qualified Data.Text.Lazy as TL
import Database.SQLite.Simple
import Database.SQLite.Simple.Types

sqlDataToText :: SQLData -> Text
sqlDataToText (SQLInteger i) = pack (show i)
sqlDataToText (SQLFloat f)   = pack (show f)
sqlDataToText (SQLText t)    = TL.fromStrict t
sqlDataToText (SQLBlob b)    = pack (show b)
sqlDataToText SQLNull        = "NULL"

registerRoutes :: Connection -> ScottyM ()
registerRoutes conn = do
    get "/inventory/getAll" $ do
        rows <- liftIO (query_ conn "SELECT * FROM Inventory" :: IO [[SQLData]])
        json (map (map sqlDataToText) rows)

    get "/inventory/getByProduct/:productId" $ do
        productId <- param "productId"
        rows <- liftIO (query conn "SELECT * FROM Inventory WHERE productId = ?" (Only (read productId :: Int)) :: IO [[SQLData]])
        json (map (map sqlDataToText) rows)
