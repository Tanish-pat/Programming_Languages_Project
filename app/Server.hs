{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
  ( scotty, get, post, put, delete, json, param, formParam, ActionM )

import Control.Monad.IO.Class (liftIO)

import Data.Aeson (Value(..), object, toJSON, (.=))
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Char8 as BS

import Database.MySQL.Simple
  ( connect, defaultConnectInfo, ConnectInfo(..), query_, execute, execute_, Query )
import Data.String (fromString)

-- | Safely run a raw SQL query and return JSON-encoded result rows
runSQLQuery :: String -> IO Value
runSQLQuery sql = do
  let baseInfo = defaultConnectInfo
        { connectHost     = "localhost"
        , connectUser     = "root"
        , connectPassword = "Khyati1998"
        , connectDatabase = "AMAZON"
        }
  conn <- connect baseInfo

  -- Raw results as [[Maybe ByteString]]
  rows <- query_ conn (fromString sql) :: IO [[Maybe BS.ByteString]]

  let jsonRows = map (map maybeBS2Val) rows
  return $ object ["rows" .= jsonRows]

-- Convert each cell to JSON
maybeBS2Val :: Maybe BS.ByteString -> Value
maybeBS2Val Nothing  = Null
maybeBS2Val (Just b) = toJSON (TE.decodeUtf8 b)

-- | Main entrypoint: start Scotty server on port 3000
main :: IO ()
main = scotty 3000 $ do
  -- === Customer Routes ===
  get "/customer/getAll" $ do
    v <- liftIO $ runSQLQuery "SELECT * FROM Customer"
    json v

  get "/customer/getById/:id" $ do
    i <- param "id" :: ActionM Int
    v <- liftIO $ runSQLQuery $ "SELECT * FROM Customer WHERE id = " ++ show i
    json v

  post "/customer/create" $ do
    name     <- formParam "name"
    email    <- formParam "email"
    age      <- formParam "age" :: ActionM Int
    isActive <- formParam "isActive" :: ActionM Bool
    let sql = "INSERT INTO Customer (name,email,age,isActive) VALUES ('"
              ++ name ++ "','" ++ email ++ "'," ++ show age ++ "," ++ show isActive ++ ")"
    v <- liftIO $ runSQLQuery sql
    json v

  put "/customer/update/:id" $ do
    i        <- param "id" :: ActionM Int
    name     <- formParam "name"
    email    <- formParam "email"
    age      <- formParam "age" :: ActionM Int
    isActive <- formParam "isActive" :: ActionM Bool
    let sql = concat ["UPDATE Customer SET ",
                      "name='",name,"', ",
                      "email='",email,"', ",
                      "age=",show age,", ",
                      "isActive=",show isActive,
                      " WHERE id=", show i]
    v <- liftIO $ runSQLQuery sql
    json v

  delete "/customer/delete/:id" $ do
    i <- param "id" :: ActionM Int
    v <- liftIO $ runSQLQuery $ "DELETE FROM Customer WHERE id = " ++ show i
    json v

  get "/customer/search" $ do
    name <- param "name" :: ActionM T.Text
    age  <- param "age" :: ActionM Int
    let sql = "SELECT * FROM Customer WHERE name LIKE '%" ++ T.unpack name ++ "%' AND age = " ++ show age
    v <- liftIO $ runSQLQuery sql
    json v

  -- === Product Routes ===
  get "/product/getAll" $ do
    v <- liftIO $ runSQLQuery "SELECT * FROM Product"
    json v

  get "/product/getBySku/:sku" $ do
    sku <- param "sku" :: ActionM T.Text
    let sql = "SELECT * FROM Product WHERE sku='" ++ T.unpack sku ++ "'"
    v <- liftIO $ runSQLQuery sql
    json v

  post "/product/create" $ do
    sku         <- formParam "sku"
    name        <- formParam "name"
    description <- formParam "description"
    price       <- formParam "price" :: ActionM Double
    tags        <- formParam "tags"
    let sql = concat ["INSERT INTO Product (sku,name,description,price,tags) VALUES ('",
                      sku,"','",name,"','",description,"',",
                      show price,
                      ",'",tags,"')"]
    v <- liftIO $ runSQLQuery sql
    json v

  put "/product/update/:sku" $ do
    sku         <- param "sku" :: ActionM T.Text
    name        <- formParam "name"
    description <- formParam "description"
    price       <- formParam "price" :: ActionM Double
    tags        <- formParam "tags"
    let sql = concat ["UPDATE Product SET name='",name,"', description='",description
                      ,"', price=",show price,
                      ", tags='",tags,"' WHERE sku='", T.unpack sku, "'"]
    v <- liftIO $ runSQLQuery sql
    json v

  delete "/product/delete/:sku" $ do
    sku <- param "sku" :: ActionM T.Text
    let sql = "DELETE FROM Product WHERE sku='" ++ T.unpack sku ++ "'"
    v <- liftIO $ runSQLQuery sql
    json v

  get "/product/search" $ do
    name <- param "name" :: ActionM T.Text
    tags <- param "tags" :: ActionM T.Text
    let sql = "SELECT * FROM Product WHERE name LIKE '%" ++ T.unpack name ++ "%' AND tags LIKE '%" ++ T.unpack tags ++ "%'"
    v <- liftIO $ runSQLQuery sql
    json v

  -- === Transaction Routes ===
  get "/transaction/getAll" $ do
    v <- liftIO $ runSQLQuery "SELECT * FROM Transaction"
    json v

  get "/transaction/getById/:transactionId" $ do
    tid <- param "transactionId" :: ActionM Int
    v <- liftIO $ runSQLQuery $ "SELECT * FROM Transaction WHERE transactionId = " ++ show tid
    json v

  post "/transaction/create" $ do
    customerId <- formParam "customerId" :: ActionM Int
    total      <- formParam "total" :: ActionM Double
    status     <- formParam "status"
    let sql = concat [ "INSERT INTO Transaction (customerId,total,status) VALUES ("
                     , show customerId, ","
                     , show total, ",'"
                     , status, "')"
                     ]

    v <- liftIO $ runSQLQuery sql
    json v

  put "/transaction/update/:transactionId" $ do
    tid        <- param "transactionId" :: ActionM Int
    customerId <- formParam "customerId" :: ActionM Int
    total      <- formParam "total" :: ActionM Double
    status     <- formParam "status"
    let sql = concat ["UPDATE Transaction SET customerId=",show customerId,
                      ", total=",show total,
                      ", status='",status,"' WHERE transactionId=",show tid]
    v <- liftIO $ runSQLQuery sql
    json v

  delete "/transaction/delete/:transactionId" $ do
    tid <- param "transactionId" :: ActionM Int
    v <- liftIO $ runSQLQuery $ "DELETE FROM Transaction WHERE transactionId = " ++ show tid
    json v

  get "/transaction/byCustomer/:customerId" $ do
    cid <- param "customerId" :: ActionM Int
    v <- liftIO $ runSQLQuery $ "SELECT * FROM Transaction WHERE customerId = " ++ show cid
    json v

  -- === Review Routes ===
  get "/review/getAll" $ do
    v <- liftIO $ runSQLQuery "SELECT * FROM Review"
    json v

  get "/review/getById/:reviewId" $ do
    rid <- param "reviewId" :: ActionM Int
    v <- liftIO $ runSQLQuery $ "SELECT * FROM Review WHERE reviewId = " ++ show rid
    json v

  post "/review/create" $ do
    customerId <- formParam "customerId" :: ActionM Int
    productId  <- formParam "productId"
    rating     <- formParam "rating" :: ActionM Int
    comment    <- formParam "comment"
    let sql = concat ["INSERT INTO Review (customerId,productId,rating,comment) VALUES (",
                      show customerId,
                      ",'",T.unpack productId,"',",
                      show rating,
                      ",'",comment,"')"]
    v <- liftIO $ runSQLQuery sql
    json v

  put "/review/update/:reviewId" $ do
    rid        <- param "reviewId" :: ActionM Int
    customerId <- formParam "customerId" :: ActionM Int
    productId  <- formParam "productId"
    rating     <- formParam "rating" :: ActionM Int
    comment    <- formParam "comment"
    let sql = concat ["UPDATE Review SET customerId=",show customerId,
                      ", productId='",T.unpack productId,"',",
                      "rating=",show rating,
                      ", comment='",comment,"' WHERE reviewId=",show rid]
    v <- liftIO $ runSQLQuery sql
    json v

  delete "/review/delete/:reviewId" $ do
    rid <- param "reviewId" :: ActionM Int
    v <- liftIO $ runSQLQuery $ "DELETE FROM Review WHERE reviewId = " ++ show rid
    json v

  get "/review/product/:productId" $ do
    pid <- param "productId" :: ActionM T.Text
    minRating <- param "minRating" :: ActionM Int
    let sql = concat ["SELECT * FROM Review WHERE productId='",T.unpack pid
                      ,"' AND rating >= ",show minRating]
    v <- liftIO $ runSQLQuery sql
    json v

  -- === Address Routes ===
  get "/address/getAll" $ do
    v <- liftIO $ runSQLQuery "SELECT * FROM Address"
    json v

  get "/address/getById/:addressId" $ do
    aid <- param "addressId" :: ActionM Int
    v <- liftIO $ runSQLQuery $ "SELECT * FROM Address WHERE addressId = " ++ show aid
    json v

  post "/address/create" $ do
    customerId <- formParam "customerId" :: ActionM Int
    line1      <- formParam "line1"
    line2      <- formParam "line2"
    city       <- formParam "city"
    state      <- formParam "state"
    zipCode    <- formParam "zipCode"
    let sql = concat ["INSERT INTO Address (customerId,line1,line2,city,state,zipCode) VALUES (",
                      show customerId,
                      ",'",line1,"','",line2,"','",city,"','",state,"','",zipCode,"')"]
    v <- liftIO $ runSQLQuery sql
    json v

  put "/address/update/:addressId" $ do
    aid        <- param "addressId" :: ActionM Int
    customerId <- formParam "customerId" :: ActionM Int
    line1      <- formParam "line1"
    line2      <- formParam "line2"
    city       <- formParam "city"
    state      <- formParam "state"
    zipCode    <- formParam "zipCode"
    let sql = concat ["UPDATE Address SET customerId=",show customerId
                      ,", line1='",line1,"', line2='",line2
                      ,"', city='",city,"', state='",state
                      ,"', zipCode='",zipCode
                      ,"' WHERE addressId=",show aid]
    v <- liftIO $ runSQLQuery sql
    json v

  delete "/address/delete/:addressId" $ do
    aid <- param "addressId" :: ActionM Int
    v <- liftIO $ runSQLQuery $ "DELETE FROM Address WHERE addressId = " ++ show aid
    json v

  get "/address/byCustomer/:customerId" $ do
    cid <- param "customerId" :: ActionM Int
    v <- liftIO $ runSQLQuery $ "SELECT * FROM Address WHERE customerId = " ++ show cid
    json v

  -- === Inventory Routes ===
  get "/inventory/getAll" $ do
    v <- liftIO $ runSQLQuery "SELECT * FROM Inventory"
    json v

  get "/inventory/getByProduct/:productId" $ do
    pid <- param "productId"
    let sql = "SELECT * FROM Inventory WHERE productId='" ++ pid ++ "'"
    v <- liftIO $ runSQLQuery sql
    json v

  put "/inventory/update/:productId" $ do
    pid      <- param "productId"
    quantity <- formParam "quantity" :: ActionM Int
    let sql = concat ["UPDATE Inventory SET quantity=", show quantity,
                      " WHERE productId='", pid, "'"]
    v <- liftIO $ runSQLQuery sql
    json v

  get "/inventory/availability" $ do
    location <- param "location"
    let sql = "SELECT * FROM Inventory WHERE location='" ++ location ++ "'"
    v <- liftIO $ runSQLQuery sql
    json v

  -- === Payment Routes ===
  get "/payment/getAll" $ do
    v <- liftIO $ runSQLQuery "SELECT * FROM Payment"
    json v

  get "/payment/getById/:paymentId" $ do
    pid <- param "paymentId" :: ActionM Int
    v <- liftIO $ runSQLQuery $ "SELECT * FROM Payment WHERE paymentId = " ++ show pid
    json v

  post "/payment/create" $ do
    transactionId <- formParam "transactionId" :: ActionM Int
    amount        <- formParam "amount" :: ActionM Double
    method        <- formParam "method"
    status        <- formParam "status"
    let sql = concat ["INSERT INTO Payment (transactionId,amount,method,status) VALUES (",
                      show transactionId,
                      ",",show amount,
                      ",'",method,"','",status,"')"]
    v <- liftIO $ runSQLQuery sql
    json v

  put "/payment/update/:paymentId" $ do
    pid           <- param "paymentId" :: ActionM Int
    transactionId <- formParam "transactionId" :: ActionM Int
    amount        <- formParam "amount" :: ActionM Double
    method        <- formParam "method"
    status        <- formParam "status"
    let sql = concat ["UPDATE Payment SET transactionId=",show transactionId
                      ,", amount=",show amount
                      ,", method='",method
                      ,"', status='",status
                      ,"' WHERE paymentId=",show pid]
    v <- liftIO $ runSQLQuery sql
    json v

  delete "/payment/delete/:paymentId" $ do
    pid <- param "paymentId" :: ActionM Int
    v <- liftIO $ runSQLQuery $ "DELETE FROM Payment WHERE paymentId = " ++ show pid
    json v

  get "/payment/byTransaction/:transactionId" $ do
    tid <- param "transactionId" :: ActionM Int
    v <- liftIO $ runSQLQuery $ "SELECT * FROM Payment WHERE transactionId = " ++ show tid
    json v

  -- === Coupon Routes ===
  get "/coupon/getAll" $ do
    v <- liftIO $ runSQLQuery "SELECT * FROM Coupon"
    json v

  get "/coupon/getByCode/:couponCode" $ do
    code <- param "couponCode"
    let sql = "SELECT * FROM Coupon WHERE couponCode='" ++ code ++ "'"
    v <- liftIO $ runSQLQuery sql
    json v

  post "/coupon/create" $ do
    code    <- formParam "couponCode"
    discount<- formParam "discount" :: ActionM Double
    isActive<- formParam "isActive" :: ActionM Bool
    let sql = concat ["INSERT INTO Coupon (couponCode,discount,isActive) VALUES ('",
                      code,"',",
                      show discount,
                      ",",show isActive,
                      ")"]
    v <- liftIO $ runSQLQuery sql
    json v

  put "/coupon/update/:couponCode" $ do
    code    <- param "couponCode"
    discount<- formParam "discount" :: ActionM Double
    isActive<- formParam "isActive" :: ActionM Bool
    let sql = concat ["UPDATE Coupon SET discount=",show discount,
                      ", isActive=",show isActive,
                      " WHERE couponCode='",code,"'"]
    v <- liftIO $ runSQLQuery sql
    json v

  delete "/coupon/delete/:couponCode" $ do
    code <- param "couponCode"
    v <- liftIO $ runSQLQuery $ "DELETE FROM Coupon WHERE couponCode='" ++ code ++ "'"
    json v

  get "/coupon/active" $ do
    v <- liftIO $ runSQLQuery "SELECT * FROM Coupon WHERE isActive = TRUE"
    json v

  -- === Category Routes ===
  get "/category/getAll" $ do
    v <- liftIO $ runSQLQuery "SELECT * FROM Category"
    json v

  get "/category/getById/:categoryId" $ do
    cid <- param "categoryId" :: ActionM Int
    v <- liftIO $ runSQLQuery $ "SELECT * FROM Category WHERE categoryId = " ++ show cid
    json v

  post "/category/create" $ do
    name        <- formParam "name"
    description <- formParam "description"
    let sql = concat ["INSERT INTO Category (name,description) VALUES ('",
                      name,"','",description,"')"]
    v <- liftIO $ runSQLQuery sql
    json v

  put "/category/update/:categoryId" $ do
    cid         <- param "categoryId" :: ActionM Int
    name        <- formParam "name"
    description <- formParam "description"
    let sql = concat ["UPDATE Category SET name='",name
                      ,"', description='",description
                      ,"' WHERE categoryId=",show cid]
    v <- liftIO $ runSQLQuery sql
    json v

  delete "/category/delete/:categoryId" $ do
    cid <- param "categoryId" :: ActionM Int
    v <- liftIO $ runSQLQuery $ "DELETE FROM Category WHERE categoryId = " ++ show cid
    json v

  -- === ProductCategory Routes ===
  get "/productCategory/getAll" $ do
    v <- liftIO $ runSQLQuery "SELECT * FROM ProductCategory"
    json v

  get "/productCategory/get/:productId/:categoryId" $ do
    pid <- param "productId"
    cid <- param "categoryId" :: ActionM Int
    let sql = concat ["SELECT * FROM ProductCategory WHERE productId='",pid
                      ,"' AND categoryId=",show cid]
    v <- liftIO $ runSQLQuery sql
    json v

  post "/productCategory/create" $ do
    pid <- formParam "productId"
    cid <- formParam "categoryId" :: ActionM Int
    let sql = concat ["INSERT INTO ProductCategory (productId,categoryId) VALUES ('",
                      pid,"',",show cid,
                      ")"]
    v <- liftIO $ runSQLQuery sql
    json v

  delete "/productCategory/delete/:productId/:categoryId" $ do
    pid <- param "productId"
    cid <- param "categoryId" :: ActionM Int
    let sql = concat ["DELETE FROM ProductCategory WHERE productId='",pid
                      ,"' AND categoryId=",show cid]
    v <- liftIO $ runSQLQuery sql
    json v

  get "/productCategory/byProduct/:productId" $ do
    pid <- param "productId"
    let sql = "SELECT * FROM ProductCategory WHERE productId='" ++ pid ++ "'"
    v <- liftIO $ runSQLQuery sql
    json v

  get "/productCategory/byCategory/:categoryId" $ do
    cid <- param "categoryId" :: ActionM Int
    let sql = "SELECT * FROM ProductCategory WHERE categoryId = " ++ show cid
    v <- liftIO $ runSQLQuery sql
    json v







-- {-# LANGUAGE OverloadedStrings #-}

-- module Main where

-- import Web.Scotty
-- import Control.Monad.IO.Class (liftIO)
-- import qualified Data.Text.Lazy as TL
-- import qualified Data.ByteString.Char8 as BS
-- import qualified Data.Text as T
-- -- import Database.MySQL.Simple
-- import Database.MySQL.Simple
--   ( connect
--   , defaultConnectInfo
--   , ConnectInfo(..)      
--   , query_
--   , Query(..)
--   , Only(..)
--   , query   
--   , query_  
--   )


-- import Data.String (fromString)

-- import Data.Aeson (Value(..), object, (.=))
-- import System.Directory (doesFileExist)
-- import Control.Exception (try, SomeException)
-- import DynamicSQLPrompt (parseSchema, Table(..), Column(..))
-- import Data.Char (toLower)

-- main :: IO ()
-- main = do
--   putStrLn "Starting server on port 3000..."
--   tables <- parseSchema "database/schema.sql"
--   scotty 3000 $ do

--     get "/ping" $
--       text "Server is up."

--     -- Run dynamic SQL sent by the frontend
--     post "/run-sql" $ do
--       sql <- param "query"
--       result <- liftIO $ runSQLQuery (TL.unpack sql)
--       json result

--     -- Endpoint to list all tables (summary)
--     get "/tables" $
--       json $ map tableSummary tables

--     -- Endpoint for detailed info about a single table
--     get "/table/:name" $ do
--       name <- param "name"
--       let mTable = findTable (T.unpack name) tables
--       case mTable of
--         Just t  -> json (tableDetail t)
--         Nothing -> json $ object ["error" .= ("Table not found" :: String)]

-- --------------------------------------------------------------------------------
-- -- Helpers
-- --------------------------------------------------------------------------------

-- runSQLQuery :: String -> IO Value
-- runSQLQuery sql = do
--   conn <- connect defaultConnectInfo
--             { connectHost     = "localhost"
--             , connectUser     = "root"
--             , connectPassword = "Khyati1998"
--             , connectDatabase = ""
--             }
--   eResult <- try (query conn (fromString sql) :: IO [Only String])  -- use query, not query_
--   case eResult of
--     Left err   -> return $ object ["error" .= show (err :: SomeException)]
--     Right rows -> return $ object ["rows"  .= map fromOnly rows]

-- --------------------------------------------------------------------------------
-- -- Table Info Helpers
-- --------------------------------------------------------------------------------

-- tableSummary :: Table -> Value
-- tableSummary t = object
--   [ "name"    .= tableName t
--   , "columns" .= map colName (tableColumns t)
--   ]

-- tableDetail :: Table -> Value
-- tableDetail t = object
--   [ "name"    .= tableName t
--   , "columns" .= map (\c -> object ["name" .= colName c, "type" .= colType c])
--                      (tableColumns t)
--   , "foreign_keys" .= map (\(col, refT, refC) ->
--                             object ["column" .= col, "ref_table" .= refT, "ref_column" .= refC])
--                           (tableForeignKeys t)
--   ]

-- findTable :: String -> [Table] -> Maybe Table
-- findTable name =
--   let lname = map toLower name
--   in  foldr (\t acc ->
--                if map toLower (tableName t) == lname
--                  then Just t
--                  else acc)
--             Nothing


