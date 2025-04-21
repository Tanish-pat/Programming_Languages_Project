{-# LANGUAGE OverloadedStrings #-}

module Links.ProductLinks (registerRoutes) where

import Web.Scotty
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy (Text, pack)
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
    get "/product/getAll" $ do
        rows <- liftIO (query_ conn "SELECT * FROM Product" :: IO [[SQLData]])
        json (map (map sqlDataToText) rows)

    get "/product/getBySku/:sku" $ do
        sku <- param "sku"
        rows <- liftIO (query conn "SELECT * FROM Product WHERE sku = ?" (Only (TL.toStrict sku)) :: IO [[SQLData]])
        json (map (map sqlDataToText) rows)
