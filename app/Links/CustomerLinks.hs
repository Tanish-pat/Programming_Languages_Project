{-# LANGUAGE OverloadedStrings #-}

module Links.CustomerLinks (registerRoutes) where

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
    get "/customer/getAll" $ do
        rows <- liftIO (query_ conn "SELECT * FROM Customer" :: IO [[SQLData]])
        json (map (map sqlDataToText) rows)

    get "/customer/getById/:id" $ do
        id <- param "id"
        rows <- liftIO (query conn "SELECT * FROM Customer WHERE id = ?" (Only (read id :: Int)) :: IO [[SQLData]])
        json (map (map sqlDataToText) rows)
