{-# LANGUAGE OverloadedStrings #-}

module Links.PaymentLinks (registerRoutes) where

import Web.Scotty
import Control.Monad.IO.Class (liftIO)
import Data.Text.Lazy (Text, pack)
import qualified Data.Text.Lazy as TL
import Database.SQLite.Simple
import Database.SQLite.Simple.Types
import PaymentRoutes
import Payment

sqlDataToText :: SQLData -> Text
sqlDataToText (SQLInteger i) = pack (show i)
sqlDataToText (SQLFloat f)   = pack (show f)
sqlDataToText (SQLText t)    = TL.fromStrict t
sqlDataToText (SQLBlob b)    = pack (show b)
sqlDataToText SQLNull        = "NULL"

registerRoutes :: Connection -> ScottyM ()
registerRoutes conn = do

    -- WORKING GET http://localhost:3000/payment/getAll
    get "/payment/getAll" $ do
        rows <- liftIO (query_ conn "SELECT * FROM Payment" :: IO [[SQLData]])
        json (map (map sqlDataToText) rows)


    -- WORKING GET http://localhost:3000/payment/get/501
    get "/payment/get/:paymentId" $ do
        pid <- param "paymentId" :: ActionM Int
        rows <- liftIO (query conn "SELECT * FROM Payment WHERE paymentId = ?" (Only pid) :: IO [[SQLData]])
        json (map (map sqlDataToText) rows)


    -- WORKING POST http://localhost:3000/payment/create?amount=300&method=GPAY&status=Failed
    post "/payment/create" $ do
        amt    <- param "amount"  :: ActionM Double
        method <- param "method"  :: ActionM Text
        status <- param "status"  :: ActionM Text
        liftIO $ execute conn "INSERT INTO Payment (amount, method, status) VALUES (?, ?, ?)" (amt, method, status)
        json (pack "Payment record created")


    -- WORKING PUT http://localhost:3000/payment/update/6?amount=300&method=GPAY&status=Failed
    put "/payment/update/:paymentId" $ do
        pid    <- param "paymentId" :: ActionM Int
        amt    <- param "amount"    :: ActionM Double
        method <- param "method"    :: ActionM Text
        status <- param "status"    :: ActionM Text
        liftIO $ execute conn "UPDATE Payment SET amount = ?, method = ?, status = ? WHERE paymentId = ?" (amt, method, status, pid)
        json (pack "Payment updated")


    -- WORKING DELETE http://localhost:3000/payment/delete/6
    delete "/payment/delete/:paymentId" $ do
        pid <- param "paymentId" :: ActionM Int
        liftIO $ execute conn "DELETE FROM Payment WHERE paymentId = ?" (Only pid)
        json (pack "Payment deleted")
