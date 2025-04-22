{-# LANGUAGE OverloadedStrings #-}

module Links.PaymentLinks (registerRoutes) where

import Web.Scotty
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import Data.Text.Lazy (Text, pack)
import qualified Data.Text.Lazy as TL
import Database.SQLite.Simple
import Database.SQLite.Simple.Types
import Database.SQLite.Simple.FromRow (FromRow(..), field)
import PaymentRoutes
import Payment

-- Utility: Convert SQLData to Text
sqlDataToText :: SQLData -> Text
sqlDataToText (SQLInteger i) = pack (show i)
sqlDataToText (SQLFloat   f) = pack (show f)
sqlDataToText (SQLText    t) = TL.fromStrict t
sqlDataToText (SQLBlob    b) = pack (show b)
sqlDataToText SQLNull        = "NULL"

-- Utility: Check if a payment exists by ID
paymentExists :: Connection -> Int -> IO Bool
paymentExists conn pid = do
    res <- query conn "SELECT 1 FROM Payment WHERE paymentId = ? LIMIT 1" (Only pid) :: IO [Only Int]
    return (not (null res))

-- Utility: Check if an identical payment already exists (for create)
isDuplicatePayment :: Connection -> Double -> Text -> Text -> IO Bool
isDuplicatePayment conn amt method status = do
    res <- query conn
        "SELECT 1 FROM Payment WHERE amount = ? AND method = ? AND status = ? LIMIT 1"
        (amt, method, status) :: IO [Only Int]
    return (not (null res))

-- Utility: Check for duplicate (excluding a given ID - used for update)
isDuplicatePaymentExcludingId :: Connection -> Int -> Double -> Text -> Text -> IO Bool
isDuplicatePaymentExcludingId conn pid amt method status = do
    res <- query conn
        "SELECT 1 FROM Payment WHERE amount = ? AND method = ? AND status = ? AND paymentId != ? LIMIT 1"
        (amt, method, status, pid) :: IO [Only Int]
    return (not (null res))

-- Scotty route registrations
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

        isDup <- liftIO $ isDuplicatePayment conn amt method status
        if isDup
            then json $ object
                [ "status"  .= ("error" :: Text)
                , "message" .= ("Duplicate payment exists. Insert aborted." :: Text)
                ]
            else do
                liftIO $ execute conn
                    "INSERT INTO Payment (amount, method, status) VALUES (?, ?, ?)"
                    (amt, method, status)
                newRow <- liftIO (query_ conn "SELECT * FROM Payment ORDER BY paymentId DESC LIMIT 1" :: IO [[SQLData]])
                json $ object
                    [ "status"  .= ("success" :: Text)
                    , "message" .= ("Payment record created" :: Text)
                    , "record"  .= map (map sqlDataToText) newRow
                    ]
    -- WORKING PUT http://localhost:3000/payment/update/501?paymentId=501&amount=300&method=GPAY&status=True
    put "/payment/update/:paymentId" $ do
        pid    <- param "paymentId" :: ActionM Int
        amt    <- param "amount"    :: ActionM Double
        method <- param "method"    :: ActionM Text
        status <- param "status"    :: ActionM Text

        exists <- liftIO $ paymentExists conn pid
        if not exists
            then json $ object
                [ "status"  .= ("error" :: Text)
                , "message" .= ("Payment record not found" :: Text)
                ]
            else do
                isDup <- liftIO $ isDuplicatePaymentExcludingId conn pid amt method status
                if isDup
                    then json $ object
                        [ "status"  .= ("error" :: Text)
                        , "message" .= ("Duplicate payment with same values exists. Update aborted." :: Text)
                        ]
                    else do
                        liftIO $ execute conn
                            "UPDATE Payment SET amount = ?, method = ?, status = ? WHERE paymentId = ?"
                            (amt, method, status, pid)
                        updated <- liftIO (query conn "SELECT * FROM Payment WHERE paymentId = ?" (Only pid) :: IO [[SQLData]])
                        json $ object
                            [ "status"  .= ("success" :: Text)
                            , "message" .= ("Payment updated" :: Text)
                            , "record"  .= map (map sqlDataToText) updated
                            ]

    -- WORKING DELETE http://localhost:3000/payment/delete/501
    delete "/payment/delete/:paymentId" $ do
        pid <- param "paymentId" :: ActionM Int

        exists <- liftIO $ paymentExists conn pid
        if not exists
            then json $ object
                [ "status"  .= ("error" :: Text)
                , "message" .= ("Payment record not found" :: Text)
                ]
            else do
                liftIO $ execute conn "DELETE FROM Payment WHERE paymentId = ?" (Only pid)
                json $ object
                    [ "status"  .= ("success" :: Text)
                    , "message" .= ("Payment deleted" :: Text)
                    ]






















-- {-# LANGUAGE OverloadedStrings #-}

-- module Links.PaymentLinks (registerRoutes) where

-- import Web.Scotty
-- import Control.Monad.IO.Class (liftIO)
-- import Data.Text.Lazy (Text, pack)
-- import qualified Data.Text.Lazy as TL
-- import Database.SQLite.Simple
-- import Database.SQLite.Simple.Types
-- import PaymentRoutes
-- import Payment

-- sqlDataToText :: SQLData -> Text
-- sqlDataToText (SQLInteger i) = pack (show i)
-- sqlDataToText (SQLFloat f)   = pack (show f)
-- sqlDataToText (SQLText t)    = TL.fromStrict t
-- sqlDataToText (SQLBlob b)    = pack (show b)
-- sqlDataToText SQLNull        = "NULL"

-- registerRoutes :: Connection -> ScottyM ()
-- registerRoutes conn = do

--     -- WORKING GET http://localhost:3000/payment/getAll
--     get "/payment/getAll" $ do
--         rows <- liftIO (query_ conn "SELECT * FROM Payment" :: IO [[SQLData]])
--         json (map (map sqlDataToText) rows)


--     -- WORKING GET http://localhost:3000/payment/get/501
--     get "/payment/get/:paymentId" $ do
--         pid <- param "paymentId" :: ActionM Int
--         rows <- liftIO (query conn "SELECT * FROM Payment WHERE paymentId = ?" (Only pid) :: IO [[SQLData]])
--         json (map (map sqlDataToText) rows)


--     -- WORKING POST http://localhost:3000/payment/create?amount=300&method=GPAY&status=Failed
--     post "/payment/create" $ do
--         amt    <- param "amount"  :: ActionM Double
--         method <- param "method"  :: ActionM Text
--         status <- param "status"  :: ActionM Text
--         liftIO $ execute conn "INSERT INTO Payment (amount, method, status) VALUES (?, ?, ?)" (amt, method, status)
--         json (pack "Payment record created")


--     -- WORKING PUT http://localhost:3000/payment/update/6?amount=300&method=GPAY&status=Failed
--     put "/payment/update/:paymentId" $ do
--         pid    <- param "paymentId" :: ActionM Int
--         amt    <- param "amount"    :: ActionM Double
--         method <- param "method"    :: ActionM Text
--         status <- param "status"    :: ActionM Text
--         liftIO $ execute conn "UPDATE Payment SET amount = ?, method = ?, status = ? WHERE paymentId = ?" (amt, method, status, pid)
--         json (pack "Payment updated")


--     -- WORKING DELETE http://localhost:3000/payment/delete/6
--     delete "/payment/delete/:paymentId" $ do
--         pid <- param "paymentId" :: ActionM Int
--         liftIO $ execute conn "DELETE FROM Payment WHERE paymentId = ?" (Only pid)
--         json (pack "Payment deleted")
