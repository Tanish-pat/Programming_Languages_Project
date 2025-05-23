{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module SchemaSeedBuilder where

import Data.String (fromString)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Database.SQLite.Simple
import qualified Data.ByteString.Char8 as BS

import ModelRegistry (models)

-- 👇 SQL Splitting Helper
splitStatements :: String -> [String]
splitStatements = filter (not . null) . map trim . splitOn ';'
  where
    splitOn c = foldr f [[]]
      where
        f x (s:ss)
          | x == c    = [] : s : ss
          | otherwise = (x : s) : ss
        f _ _ = [[]]
    trim = T.unpack . T.strip . T.pack

-- 👇 Template Haskell entry point
genSchemaAndSeed :: Q [Dec]
genSchemaAndSeed = do
  runIO $ do
    let dir = "database"
    createDirectoryIfMissing True dir
    let schemaSQL = generateSchema models
    let seedSQL   = generateSeed models

    writeFile (dir </> "schema.sql") schemaSQL
    writeFile (dir </> "seed.sql")   seedSQL

    -- Recreate the SQLite database
    let dbPath = dir </> "INVENTORY.db"
    conn <- open dbPath

    -- Drop existing tables (optional, no DROP DATABASE in SQLite)
    -- Execute schema and seed
    mapM_ (execute_ conn . fromString) (splitStatements schemaSQL)
    mapM_ (execute_ conn . fromString) (splitStatements seedSQL)

    close conn
  return []

-- 👇 Schema Generator
generateSchema :: [(String, [(String, Name)])] -> String
generateSchema = concatMap tableToSQL
  where
    commaSepLines :: [String] -> String
    commaSepLines xs = unlines (map (++ ",") (init xs) ++ [last xs])

    tableToSQL (name, cols) =
      "CREATE TABLE IF NOT EXISTS " ++ quote name ++ " (\n" ++
      commaSepLines (map columnLine cols ++ constraints name cols) ++
      ");\n\n"

    columnLine (colName, colType) =
      "  " ++ colName ++ " " ++ mapType colType

    mapType t
      | t == ''Int    = "INTEGER"
      | t == ''T.Text = "TEXT"
      | t == ''Double = "REAL"
      | t == ''Bool   = "BOOLEAN"
      | otherwise     = "TEXT"

    quote n = "\"" ++ n ++ "\""

    constraints "Customer" _ =
      ["  PRIMARY KEY (id)"]
    constraints "Product" _ =
      ["  PRIMARY KEY (sku)"]
    constraints "Review" _ =
      [ "  PRIMARY KEY (reviewId)"
      , "  FOREIGN KEY (customerId) REFERENCES Customer(id)"
      , "  FOREIGN KEY (productId) REFERENCES Product(sku)"
      ]
    constraints "Address" _ =
      [ "  PRIMARY KEY (addressId)"
      , "  FOREIGN KEY (customerId) REFERENCES Customer(id)"
      ]
    constraints "Inventory" _ =
      [ "  PRIMARY KEY (productId)"
      , "  FOREIGN KEY (productId) REFERENCES Product(sku)"
      ]
    constraints "Payment" _ =
      [ "  PRIMARY KEY (paymentId)"
      ]
    constraints "Coupon" _ =
      [ "  PRIMARY KEY (couponCode)"
      ]
    constraints "Category" _ =
      [ "  PRIMARY KEY (categoryId)"
      ]
    constraints "ProductCategory" _ =
      [ "  PRIMARY KEY (productId, categoryId)"
      , "  FOREIGN KEY (productId) REFERENCES Product(sku)"
      , "  FOREIGN KEY (categoryId) REFERENCES Category(categoryId)"
      ]
    constraints _ _ = []

-- 👇 Seed Generator
generateSeed :: [(String, [(String, Name)])] -> String
generateSeed models = unlines $ concatMap tableSeed models
  where
    tableSeed ("Customer", _) =
      [ "INSERT INTO Customer VALUES (1, 'Arjun', 'arjun@example.com', 30, TRUE);"
      , "INSERT INTO Customer VALUES (2, 'Priya', 'priya@example.com', 28, TRUE);"
      , "INSERT INTO Customer VALUES (3, 'Ravi', 'ravi@example.com', 35, FALSE);"
      , "INSERT INTO Customer VALUES (4, 'Anjali', 'anjali@example.com', 25, TRUE);"
      , "INSERT INTO Customer VALUES (5, 'Vikram', 'vikram@example.com', 40, TRUE);"
      ]
    tableSeed ("Product", _) =
      [ "INSERT INTO Product VALUES ('SKU001', 'Chai', 'Premium Indian tea', 150.0, 'beverage');"
      , "INSERT INTO Product VALUES ('SKU002', 'Masala', 'Mixed spices', 75.0, 'grocery');"
      , "INSERT INTO Product VALUES ('SKU003', 'Kurkure', 'Spicy snack', 20.0, 'snack');"
      , "INSERT INTO Product VALUES ('SKU004', 'Basmati Rice', 'Long grain rice', 90.0, 'grain');"
      , "INSERT INTO Product VALUES ('SKU005', 'Pickle', 'Mango pickle jar', 60.0, 'condiment');"
      ]
    tableSeed ("Review", _) =
      [ "INSERT INTO Review VALUES (1, 1, 'SKU001', 5, 'Amazing tea!');"
      , "INSERT INTO Review VALUES (2, 2, 'SKU002', 4, 'Loved the spices');"
      , "INSERT INTO Review VALUES (3, 3, 'SKU003', 3, 'Too spicy for me');"
      , "INSERT INTO Review VALUES (4, 4, 'SKU004', 5, 'Best rice I ever bought');"
      , "INSERT INTO Review VALUES (5, 5, 'SKU005', 4, 'Nice and tangy');"
      ]
    tableSeed ("Address", _) =
      [ "INSERT INTO Address VALUES (1, 1, '12 MG Road', 'Near Mall', 'Mumbai', 'MH', '400001');"
      , "INSERT INTO Address VALUES (2, 2, '45 Ring Road', '', 'Delhi', 'DL', '110001');"
      , "INSERT INTO Address VALUES (3, 3, '89 Residency Rd', 'Apt 5B', 'Bangalore', 'KA', '560025');"
      , "INSERT INTO Address VALUES (4, 4, '78 Canal Street', '3rd Floor', 'Kolkata', 'WB', '700091');"
      , "INSERT INTO Address VALUES (5, 5, '21 Nungambakkam', '', 'Chennai', 'TN', '600034');"
      ]
    tableSeed ("Inventory", _) =
      [ "INSERT INTO Inventory VALUES ('SKU001', 100, 'Mumbai');"
      , "INSERT INTO Inventory VALUES ('SKU002', 50, 'Delhi');"
      , "INSERT INTO Inventory VALUES ('SKU003', 200, 'Bangalore');"
      , "INSERT INTO Inventory VALUES ('SKU004', 150, 'Chennai');"
      , "INSERT INTO Inventory VALUES ('SKU005', 90, 'Hyderabad');"
      ]
    tableSeed ("Payment", _) =
      [ "INSERT INTO Payment VALUES (501, 230.0, 'UPI', 'Completed');"
      , "INSERT INTO Payment VALUES (502, 150.0, 'Credit Card', 'Pending');"
      , "INSERT INTO Payment VALUES (503, 75.0, 'NetBanking', 'Completed');"
      , "INSERT INTO Payment VALUES (504, 120.0, 'UPI', 'Failed');"
      , "INSERT INTO Payment VALUES (505, 200.0, 'Cash on Delivery', 'Completed');"
      ]
    tableSeed ("Coupon", _) =
      [ "INSERT INTO Coupon VALUES ('NEWcustomer10', 10.0, TRUE);"
      , "INSERT INTO Coupon VALUES ('SUMMER15', 15.0, TRUE);"
      , "INSERT INTO Coupon VALUES ('FESTIVE20', 20.0, FALSE);"
      , "INSERT INTO Coupon VALUES ('SUPER25', 25.0, TRUE);"
      , "INSERT INTO Coupon VALUES ('FLAT30', 30.0, FALSE);"
      ]
    tableSeed ("Category", _) =
      [ "INSERT INTO Category VALUES (1, 'Beverages', 'Drinks like tea, coffee etc.');"
      , "INSERT INTO Category VALUES (2, 'Spices', 'Indian spices');"
      , "INSERT INTO Category VALUES (3, 'Snacks', 'Crunchy Indian snacks');"
      , "INSERT INTO Category VALUES (4, 'Grains', 'Rice, Wheat etc.');"
      , "INSERT INTO Category VALUES (5, 'Condiments', 'Pickles and sauces');"
      ]
    tableSeed ("ProductCategory", _) =
      [ "INSERT INTO ProductCategory VALUES ('SKU001', 1);"
      , "INSERT INTO ProductCategory VALUES ('SKU002', 2);"
      , "INSERT INTO ProductCategory VALUES ('SKU003', 3);"
      , "INSERT INTO ProductCategory VALUES ('SKU004', 4);"
      , "INSERT INTO ProductCategory VALUES ('SKU005', 5);"
      ]
    tableSeed _ = []
