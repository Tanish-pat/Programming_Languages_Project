{-# LANGUAGE OverloadedStrings #-}
module QueryPrompter
  ( Table(..)
  , Operation(..)
  , promptTable
  , promptOperation
  , promptValues
  , generateSQL
  ) where

import System.Console.ANSI
import System.IO
import Control.Monad (forM)
import Data.List (intercalate)
import qualified Data.Map as M

-- |
-- Enumerate your tables
data Table
  = Customer
  | Product
  | Review
  | Address
  | Inventory
  | Payment
  | Coupon
  | Category
  | ProductCategory
  deriving (Show, Read, Enum, Bounded, Eq, Ord)

-- |
-- The 5 basic operations
data Operation = Insert | Update | Delete | GetAll | GetByID
  deriving (Show, Read, Enum, Bounded, Eq)

-- | Human‑friendly table menu
-- | Human‑friendly table menu with an Exit option
promptTable :: IO (Maybe Table)
promptTable = do
  setSGR [SetColor Foreground Vivid Cyan]
  putStrLn "Select a table:"
  mapM_ (\(i, t) -> putStrLn $ show i ++ ". " ++ show t) (zip [1..] allTables)
  putStrLn $ show (length allTables + 1) ++ ". Exit"
  setSGR [Reset]
  idx <- readLn
  if idx == length allTables + 1
    then return Nothing
    else return $ Just (allTables !! (idx - 1))
  where
    allTables = [minBound..maxBound] :: [Table]


-- | Human‑friendly operation menu
promptOperation :: IO Operation
promptOperation = do
  setSGR [SetColor Foreground Vivid Cyan]
  putStrLn "Select operation:"
  mapM_ (\(i,op) -> putStrLn $ show i ++ ". " ++ show op) (zip [1..] allOps)
  setSGR [Reset]
  idx <- readLn
  return $ allOps !! (idx-1)
  where
    allOps = [minBound..maxBound] :: [Operation]

-- | Your schema: columns and PKs for each table
-- | Column types for display prompts
columnTypes :: M.Map Table (M.Map String String)
columnTypes = M.fromList
  [ (Customer, M.fromList
      [ ("id", "Int")
      , ("name", "Text")
      , ("email", "Text")
      , ("age", "Int")
      , ("isActive", "Bool")
      ])
  , (Product, M.fromList
      [ ("sku", "Text")
      , ("name", "Text")
      , ("description", "Text")
      , ("price", "Float")
      , ("tags", "Text")
      ])
  , (Review, M.fromList
      [ ("reviewId", "Int")
      , ("customerId", "Int")
      , ("productId", "Text")
      , ("rating", "Int")
      , ("comment", "Text")
      ])
  , (Address, M.fromList
      [ ("addressId", "Int")
      , ("customerId", "Int")
      , ("line1", "Text")
      , ("line2", "Text")
      , ("city", "Text")
      , ("state", "Text")
      , ("zipCode", "Text")
      ])
  , (Inventory, M.fromList
      [ ("productId", "Text")
      , ("quantity", "Int")
      , ("location", "Text")
      ])
  , (Payment, M.fromList
      [ ("paymentId", "Int")
      , ("amount", "Float")
      , ("method", "Text")
      , ("status", "Text")
      ])
  , (Coupon, M.fromList
      [ ("couponCode", "Text")
      , ("discount", "Float")
      , ("isActive", "Bool")
      ])
  , (Category, M.fromList
      [ ("categoryId", "Int")
      , ("name", "Text")
      , ("description", "Text")
      ])
  , (ProductCategory, M.fromList
      [ ("productId", "Text")
      , ("categoryId", "Int")
      ])
  ]

-- | Column names for each table
tableCols :: M.Map Table [String]
tableCols = M.fromList
  [ (Customer,       ["id","name","email","age","isActive"])
  , (Product,        ["sku","name","description","price","tags"])
  , (Review,         ["reviewId","customerId","productId","rating","comment"])
  , (Address,        ["addressId","customerId","line1","line2","city","state","zipCode"])
  , (Inventory,      ["productId","quantity","location"])
  , (Payment,        ["paymentId","amount","method","status"])
  , (Coupon,         ["couponCode","discount","isActive"])
  , (Category,       ["categoryId","name","description"])
  , (ProductCategory,["productId","categoryId"])
  ]



pkCols :: M.Map Table [String]
pkCols = M.fromList
  [ (Customer,        ["id"])
  , (Product,         ["sku"])
  , (Review,          ["reviewId"])
  , (Address,         ["addressId"])
  , (Inventory,       ["productId"])
  , (Payment,         ["paymentId"])
  , (Coupon,          ["couponCode"])
  , (Category,        ["categoryId"])
  , (ProductCategory, ["productId","categoryId"])
  ]

-- | Prompt exactly the fields you need
promptValues :: Table -> Operation -> IO [(String, String)]
promptValues tbl op = do
  let cols   = tableCols M.! tbl
      pks    = pkCols M.! tbl
      nonPKs = filter (`notElem` pks) cols

      -- which fields to ask for
      ask = case op of
        Insert -> cols
        Update -> pks ++ nonPKs
        Delete -> pks
        GetByID-> pks
        GetAll  -> []

  -- collect
  forM ask $ \col -> do
    let dtype = maybe "?" id (M.lookup tbl columnTypes >>= M.lookup col)
    setSGR [SetColor Foreground Vivid Green]
    putStr $ "  " ++ col
    setSGR [SetColor Foreground Dull White]
    putStr $ " (" ++ dtype ++ "): "
    setSGR [Reset]
    

    hFlush stdout
    val <- getLine
    setSGR [Reset]
    return (col, val)

-- | Build the SQL string
generateSQL :: Table -> Operation -> [(String,String)] -> String
generateSQL tbl op kvs =
  let tblName = show tbl
      m        = M.fromList kvs

      mkList xs f = intercalate ", " $ f <$> xs
      qcols     = tableCols M.! tbl
      pks       = pkCols M.! tbl
      nonPKs    = filter (`notElem` pks) qcols

      esc v     = "'" ++ v ++ "'"
      val c     = esc (m M.! c)

      whereClause = if null pks
        then ""
        else " WHERE " ++
             intercalate " AND " [c ++ "=" ++ (m M.! c) | c <- pks]

  in case op of
       Insert ->
         "INSERT INTO " ++ tblName ++
         " (" ++ mkList qcols id ++ ") VALUES (" ++
         mkList qcols val ++ ");"

       Update ->
         "UPDATE " ++ tblName ++
         " SET " ++ mkList nonPKs (\c-> c ++ "=" ++ val c) ++
         whereClause ++ ";"

       Delete ->
         "DELETE FROM " ++ tblName ++ whereClause ++ ";"

       GetAll ->
         "SELECT * FROM " ++ tblName ++ ";"

       GetByID ->
         "SELECT * FROM " ++ tblName ++ whereClause ++ ";"
