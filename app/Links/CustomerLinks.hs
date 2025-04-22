{-# LANGUAGE OverloadedStrings #-}

module Links.CustomerLinks (registerRoutes) where

import Web.Scotty
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import Data.Text.Lazy (Text, pack)
import qualified Data.Text.Lazy as TL
import Database.SQLite.Simple
import Database.SQLite.Simple.Types
import Database.SQLite.Simple.FromRow (FromRow(..), field)
import CustomerRoutes
import Customer

sqlDataToText :: SQLData -> Text
sqlDataToText (SQLInteger i) = pack (show i)
sqlDataToText (SQLFloat   f) = pack (show f)
sqlDataToText (SQLText    t) = TL.fromStrict t
sqlDataToText (SQLBlob    b) = pack (show b)
sqlDataToText SQLNull        = "NULL"

-- Check if customer exists by email
customerExists :: Connection -> Text -> IO Bool
customerExists conn email = do
    res <- query conn "SELECT 1 FROM Customer WHERE email = ? LIMIT 1" (Only email) :: IO [Only Int]
    return (not (null res))

-- Check if customer exists by id
customerExistsById :: Connection -> Int -> IO Bool
customerExistsById conn cid = do
    res <- query conn "SELECT 1 FROM Customer WHERE id = ? LIMIT 1" (Only cid) :: IO [Only Int]
    return (not (null res))

registerRoutes :: Connection -> ScottyM ()
registerRoutes conn = do

    -- WORKING GET http://localhost:3000/customer/getAll
    get "/customer/getAll" $ do
        rows <- liftIO (query_ conn
            "SELECT * FROM Customer" :: IO [[SQLData]])
        json (map (map sqlDataToText) rows)

    -- WORKING GET http://localhost:3000/customer/getById/2
    get "/customer/getById/:id" $ do
        cid  <- param "id"
        rows <- liftIO (query conn
            "SELECT * FROM Customer WHERE id = ?" (Only (read cid :: Int))
            :: IO [[SQLData]])
        json (map (map sqlDataToText) rows)

    -- WORKING POST http://localhost:3000/customer/create?name=cvercefc&email=ofuy4rbck&age=23&isActive=False
    post "/customer/create" $ do
        name     <- param "name"
        email    <- param "email"
        age      <- param "age"
        isActive <- param "isActive"

        -- Check if the customer already exists
        exists <- liftIO $ customerExists conn email
        if exists
            then json $ object ["status" .= ("error" :: Text), "message" .= ("Customer with this email already exists" :: Text)]
            else do
                liftIO $ execute conn
                    "INSERT INTO Customer (name, email, age, isActive) VALUES (?, ?, ?, ?)"
                    (name :: Text, email :: Text, age :: Int, isActive :: Bool)
                -- return the newly‐inserted record
                newRow <- liftIO (query_ conn
                    "SELECT * FROM Customer ORDER BY id DESC LIMIT 1" :: IO [[SQLData]])
                json (map (map sqlDataToText) newRow)

    -- WORKING  PUT http://localhost:3000/customer/update/1?id=1&name=cewcwecwa&email=eververvseve&age=43&isActive=True
    put "/customer/update/:id" $ do
        cid      <- param "id"
        name     <- param "name"
        email    <- param "email"
        age      <- param "age"
        isActive <- param "isActive"

        -- Check if the customer exists
        exists <- liftIO $ customerExistsById conn (read cid :: Int)
        if not exists
            then json $ object ["status" .= ("error" :: Text), "message" .= ("Customer not found" :: Text)]
            else do
                liftIO $ execute conn
                    "UPDATE Customer SET name = ?, email = ?, age = ?, isActive = ? WHERE id = ?"
                    (name :: Text, email :: Text, age :: Int, isActive :: Bool, read cid :: Int)
                updated <- liftIO (query conn
                    "SELECT * FROM Customer WHERE id = ?" (Only (read cid :: Int))
                    :: IO [[SQLData]])
                json (map (map sqlDataToText) updated)

    -- WORKING DELETE http://localhost:3000/customer/delete/1?id=1
    delete "/customer/delete/:id" $ do
        cid <- param "id"

        -- Check if the customer exists before attempting to delete
        exists <- liftIO $ customerExistsById conn (read cid :: Int)
        if not exists
            then json $ object ["status" .= ("error" :: Text), "message" .= ("Customer not found" :: Text)]
            else do
                liftIO $ execute conn
                    "DELETE FROM Customer WHERE id = ?" (Only (read cid :: Int))
                json (object ["status" .= ("success" :: Text), "message" .= ("Customer deleted" :: Text)])

    -- WORKING GET http://localhost:3000/customer/age/10/50
    get "/customer/age/:min/:max" $ do
        minA <- param "min"
        maxA <- param "max"
        rows <- liftIO (query conn
            "SELECT * FROM Customer WHERE age BETWEEN ? AND ?"
            ( (read minA :: Int), (read maxA :: Int) )
            :: IO [[SQLData]])
        json (map (map sqlDataToText) rows)

    -- WORKING GET http://localhost:3000/customer/active/1?flag=true
    get "/customer/active/:flag" $ do
        flag <- param "flag"
        let isAct = case (flag :: Text) of
                        "true"  -> True
                        "false" -> False
                        _       -> False
        rows <- liftIO (query conn
            "SELECT * FROM Customer WHERE isActive = ?" (Only isAct)
            :: IO [[SQLData]])
        json (map (map sqlDataToText) rows)
