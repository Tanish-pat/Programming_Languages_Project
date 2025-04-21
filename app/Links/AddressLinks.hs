{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Links.AddressLinks (registerRoutes) where

import Web.Scotty
import Web.Scotty.Internal.Types (RoutePattern)  -- Explicit import
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Network.HTTP.Types (status404)
import AddressRoutes
import Address

instance FromRow Address where
    fromRow = Address <$> field <*> field <*> field <*> field <*> field <*> field <*> field

registerRoutes :: Connection -> ScottyM ()
registerRoutes conn = do
    -- Convert to RoutePattern with literal + T.unpack
    get (literal $ T.unpack getAllAddresses) $ do
        addresses <- liftIO (query_ conn "SELECT * FROM Address" :: IO [Address])
        json addresses

    get (literal $ T.unpack (getByAddressId ":addressId")) $ do
        addressId <- param "addressId" :: ActionM Int
        addresses <- liftIO (query conn "SELECT * FROM Address WHERE addressId = ?" (Only addressId) :: IO [Address])
        case addresses of
            [] -> status status404 >> json ("Address not found" :: Text)
            (addr:_) -> json addr

    get (literal $ T.unpack (getByCustomerId ":customerId")) $ do
        customerId <- param "customerId" :: ActionM Int
        addresses <- liftIO (query conn "SELECT * FROM Address WHERE customerId = ?" (Only customerId) :: IO [Address])
        json addresses

















-- {-# LANGUAGE OverloadedStrings #-}

-- module Links.AddressLinks (registerRoutes) where

-- import Web.Scotty
-- import Control.Monad.IO.Class (liftIO)
-- import Data.Text.Lazy (Text, pack)
-- import Database.SQLite.Simple
-- import Database.SQLite.Simple.Types
-- import AddressRoutes
-- import Address

-- sqlDataToText :: SQLData -> Text
-- sqlDataToText (SQLInteger i) = pack (show i)
-- sqlDataToText (SQLFloat f)   = pack (show f)
-- sqlDataToText (SQLText t)    = t  -- no need to convert `Text` to `Text`
-- sqlDataToText (SQLBlob b)    = pack (show b)
-- sqlDataToText SQLNull        = "NULL"

-- registerRoutes :: Connection -> ScottyM ()
-- registerRoutes conn = do
--     -- Get all addresses
--     get getAllAddresses $ do
--         rows <- liftIO (query_ conn "SELECT * FROM Address" :: IO [Address])
--         json rows

--     -- Get address by addressId
--     get (getByAddressId ":addressId") $ do
--         addressId <- param "addressId" :: ActionM Int
--         addresses <- liftIO (query conn "SELECT * FROM Address WHERE addressId = ?" (Only addressId) :: IO [Address])
--         case addresses of
--             [] -> status status404 >> json ("Address not found" :: Text)
--             (addr : _) -> json addr

--     -- Get addresses by customerId
--     get (getByCustomerId ":customerId") $ do
--         customerId <- param "customerId" :: ActionM Int
--         addresses <- liftIO (query conn "SELECT * FROM Address WHERE customerId = ?" (Only customerId) :: IO [Address])
--         json addresses

--     -- Create address (you can extend with additional functionality to insert data)
--     post createAddress $ do
--         -- Add logic to create address (e.g., reading JSON data and inserting into DB)
--         json (pack "Address creation not implemented")

--     -- Update address by addressId (explicitly specify Integer type)
--     put (updateAddress ":addressId") $ do
--         addressId <- param "addressId" :: ActionM Int
--         -- Update logic
--         json (pack ("Address updated with ID: " <> show addressId))

--     -- Delete address by addressId (explicitly specify Integer type)
--     delete (deleteAddress ":addressId") $ do
--         addressId <- param "addressId" :: ActionM Int
--         -- Delete logic
--         json (pack ("Address deleted with ID: " <> show addressId))
















-- {-# LANGUAGE OverloadedStrings #-}

-- module Links.AddressLinks (registerRoutes) where

-- import Web.Scotty
-- import Control.Monad.IO.Class (liftIO)
-- import Data.Text.Lazy (Text, pack)
-- import qualified Data.Text.Lazy as TL
-- import Database.SQLite.Simple
-- import Database.SQLite.Simple.Types
-- import AddressRoutes
-- import Address

-- sqlDataToText :: SQLData -> Text
-- sqlDataToText (SQLInteger i) = pack (show i)
-- sqlDataToText (SQLFloat f)   = pack (show f)
-- sqlDataToText (SQLText t)    = TL.fromStrict t
-- sqlDataToText (SQLBlob b)    = pack (show b)
-- sqlDataToText SQLNull        = "NULL"

-- registerRoutes :: Connection -> ScottyM ()
-- registerRoutes conn = do
--     get "/address/getAll" $ do
--         rows <- liftIO (query_ conn "SELECT * FROM Address" :: IO [[SQLData]])
--         json (map (map sqlDataToText) rows)

--     get "/address/getById/:addressId" $ do
--         addressId <- param "addressId"
--         rows <- liftIO (query conn "SELECT * FROM Address WHERE addressId = ?" (Only (read addressId :: Int)) :: IO [[SQLData]])
--         json (map (map sqlDataToText) rows)

--     get "/address/byCustomer/:customerId" $ do
--         customerId <- param "customerId"
--         rows <- liftIO (query conn "SELECT * FROM Address WHERE customerId = ?" (Only (read customerId :: Int)) :: IO [[SQLData]])
--         json (map (map sqlDataToText) rows)
