{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Web.Scotty
import Control.Monad.IO.Class (liftIO)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Data.Text.Lazy (Text, pack)
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import Database.SQLite.Simple (open, query_, SQLData(..), Connection)

sqlDataToText :: SQLData -> Text
sqlDataToText (SQLInteger i) = pack (show i)
sqlDataToText (SQLFloat f)   = pack (show f)
sqlDataToText (SQLText t)    = TL.fromStrict t
sqlDataToText (SQLBlob b)    = pack (show b)
sqlDataToText SQLNull        = "NULL"

-- SQLite database path
dbPath :: String
dbPath = "database/AMAZON.db"

main :: IO ()
main = do
    conn <- open dbPath
    putStrLn "🚀 Starting server on http://localhost:3000 ..."
    scotty 3000 $ do
        middleware logStdoutDev

        get "/" $ do
            text "WELCOME TO THE INVENTORY MANAGEMENT SYSTEM"

        -- CUSTOMER ROUTES
        get "/customer/getAll" $ do
            rows <- liftIO (query_ conn "SELECT * FROM Customer" :: IO [[SQLData]])
            let textRows = map (map sqlDataToText) rows
            json textRows
