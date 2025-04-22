{-# LANGUAGE OverloadedStrings #-}

module Links.AddressLinks (registerRoutes) where

import Web.Scotty
import Control.Monad.IO.Class (liftIO)
import Data.Text.Lazy (Text, pack)
import qualified Data.Text.Lazy as TL
import Database.SQLite.Simple
import Database.SQLite.Simple.Types
import AddressRoutes
import Address

sqlDataToText :: SQLData -> Text
sqlDataToText (SQLInteger i) = pack (show i)
sqlDataToText (SQLFloat   f) = pack (show f)
sqlDataToText (SQLText    t) = TL.fromStrict t
sqlDataToText (SQLBlob    b) = pack (show b)
sqlDataToText SQLNull        = "NULL"

registerRoutes :: Connection -> ScottyM ()
registerRoutes conn = do

    -- WORKING GET http://localhost:3000/address/getAll
    get "/address/getAll" $ do
        rows <- liftIO (query_ conn "SELECT * FROM Address" :: IO [[SQLData]])
        json (map (map sqlDataToText) rows)

    -- WORKING GET http://localhost:3000/address/getById/1
    get "/address/getById/:addressId" $ do
        addressId <- param "addressId" :: ActionM Int
        rows <- liftIO (query conn "SELECT * FROM Address WHERE addressId = ?" (Only addressId) :: IO [[SQLData]])
        json (map (map sqlDataToText) rows)

    -- WORKING GET http://localhost:3000/address/byCustomer/1
    get "/address/byCustomer/:customerId" $ do
        customerId <- param "customerId" :: ActionM Int
        rows <- liftIO (query conn "SELECT * FROM Address WHERE customerId = ?" (Only customerId) :: IO [[SQLData]])
        json (map (map sqlDataToText) rows)

    -- WORKING POST http://localhost:3000/address/create?customerId=1&line1=idhar hi hai&line2=paas mein hi hai&city=bangalot&state=kalo ki state&zipCode=192938
    post "/address/create" $ do
        customerId <- param "customerId" :: ActionM Int
        line1      <- param "line1"      :: ActionM Text
        line2      <- param "line2"      :: ActionM Text
        city       <- param "city"       :: ActionM Text
        state      <- param "state"      :: ActionM Text
        zipCode    <- param "zipCode"    :: ActionM Text
        liftIO $ execute conn
            "INSERT INTO Address (customerId, line1, line2, city, state, zipCode) VALUES (?, ?, ?, ?, ?, ?)"
            (customerId, line1, line2, city, state, zipCode)
        newRow <- liftIO (query_ conn
            "SELECT * FROM Address ORDER BY addressId DESC LIMIT 1" :: IO [[SQLData]])
        json (map (map sqlDataToText) newRow)

    -- WORKING PUT http://localhost:3000/address/update/1?addressId=1&customerId=1&line1=idhar hi hai&line2=paas mein hi hai&city=bangalot&state=kalo ki state&zipCode=192938
    put "/address/update/:addressId" $ do
        addressId  <- param "addressId"  :: ActionM Int
        customerId <- param "customerId" :: ActionM Int
        line1      <- param "line1"      :: ActionM Text
        line2      <- param "line2"      :: ActionM Text
        city       <- param "city"       :: ActionM Text
        state      <- param "state"      :: ActionM Text
        zipCode    <- param "zipCode"    :: ActionM Text
        liftIO $ execute conn
            "UPDATE Address SET customerId = ?, line1 = ?, line2 = ?, city = ?, state = ?, zipCode = ? WHERE addressId = ?"
            (customerId, line1, line2, city, state, zipCode, addressId)
        updated <- liftIO (query conn
            "SELECT * FROM Address WHERE addressId = ?" (Only addressId) :: IO [[SQLData]])
        json (map (map sqlDataToText) updated)

    -- WORKING DELETE http://localhost:3000/address/delete/1
    delete "/address/delete/:addressId" $ do
        addressId <- param "addressId" :: ActionM Int
        liftIO $ execute conn "DELETE FROM Address WHERE addressId = ?" (Only addressId)
        json (pack $ "Deleted addressId=" ++ show addressId)