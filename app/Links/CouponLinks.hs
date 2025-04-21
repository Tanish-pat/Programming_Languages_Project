{-# LANGUAGE OverloadedStrings #-}

module Links.CouponLinks (registerRoutes) where

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
    get "/coupon/getAll" $ do
        rows <- liftIO (query_ conn "SELECT * FROM Coupon" :: IO [[SQLData]])
        json (map (map sqlDataToText) rows)

    get "/coupon/getByCode/:couponCode" $ do
        couponCode <- param "couponCode"
        rows <- liftIO (query conn "SELECT * FROM Coupon WHERE couponCode = ?" (Only (TL.toStrict couponCode)) :: IO [[SQLData]])
        json (map (map sqlDataToText) rows)
