{-# LANGUAGE OverloadedStrings #-}

module Links.ReviewLinks (registerRoutes) where

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
    get "/review/getAll" $ do
        rows <- liftIO (query_ conn "SELECT * FROM Review" :: IO [[SQLData]])
        json (map (map sqlDataToText) rows)

    get "/review/getById/:reviewId" $ do
        reviewId <- param "reviewId"
        rows <- liftIO (query conn "SELECT * FROM Review WHERE reviewId = ?" (Only (read reviewId :: Int)) :: IO [[SQLData]])
        json (map (map sqlDataToText) rows)

    get "/review/product/:productId" $ do
        productId <- param "productId"
        rows <- liftIO (query conn "SELECT * FROM Review WHERE productId = ?" (Only (read productId :: Int)) :: IO [[SQLData]])
        json (map (map sqlDataToText) rows)
