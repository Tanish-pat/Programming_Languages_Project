{-# LANGUAGE OverloadedStrings #-}

module Links.AddressLinks (registerRoutes) where

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
    get "/address/getAll" $ do
        rows <- liftIO (query_ conn "SELECT * FROM Address" :: IO [[SQLData]])
        json (map (map sqlDataToText) rows)

    get "/address/getById/:addressId" $ do
        addressId <- param "addressId"
        rows <- liftIO (query conn "SELECT * FROM Address WHERE addressId = ?" (Only (read addressId :: Int)) :: IO [[SQLData]])
        json (map (map sqlDataToText) rows)

    get "/address/byCustomer/:customerId" $ do
        customerId <- param "customerId"
        rows <- liftIO (query conn "SELECT * FROM Address WHERE customerId = ?" (Only (read customerId :: Int)) :: IO [[SQLData]])
        json (map (map sqlDataToText) rows)
