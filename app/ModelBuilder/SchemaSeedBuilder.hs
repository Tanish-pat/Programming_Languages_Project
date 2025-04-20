{-# LANGUAGE TemplateHaskell #-}

module SchemaSeedBuilder where


import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

-- 👇 Import your model registry here!
import ModelRegistry (models)


-- 👇 Template Haskell function
genSchemaAndSeed :: Q [Dec]
genSchemaAndSeed = do
  runIO $ do
    let dir = "database"
    createDirectoryIfMissing True dir

    writeFile (dir </> "schema.sql") (generateSchema models)
    writeFile (dir </> "seed.sql")   (generateSeed models)

  return []  

  -- SQL GENERATOR

generateSchema :: [(String, [(String, Name)])] -> String
generateSchema = concatMap tableToSQL
  where
    tableToSQL (name, cols) =
      "CREATE TABLE " ++ quote name ++ " (\n" ++
      unlines (map columnLine cols ++ constraints name cols) ++
      ");\n\n"

    columnLine (colName, colType) =
      "  " ++ colName ++ " " ++ mapType colType ++ ","

    mapType t
      | t == ''Int    = "INTEGER"
      | t == ''T.Text = "TEXT"
      | t == ''Double = "REAL"
      | t == ''Bool   = "BOOLEAN"
      | otherwise     = "TEXT"

    -- Quote reserved names like Order
    quote "Order" = "\"Order\""
    quote n = n

    -- 🔗 Constraints including primary/foreign keys
    constraints "User" cols =
      ["  PRIMARY KEY (id)"]
    constraints "Product" cols =
      ["  PRIMARY KEY (sku)"]
    constraints "Order" cols =
      [ "  PRIMARY KEY (orderId)"
      , "  FOREIGN KEY (userId) REFERENCES User(id)"
      ]
    constraints "Review" cols =
      [ "  PRIMARY KEY (reviewId)"
      , "  FOREIGN KEY (userId) REFERENCES User(id)"
      , "  FOREIGN KEY (productId) REFERENCES Product(sku)"
      ]
    constraints "Address" cols =
      [ "  PRIMARY KEY (addressId)"
      , "  FOREIGN KEY (userId) REFERENCES User(id)"
      ]
    constraints "Inventory" cols =
      [ "  PRIMARY KEY (productId)"
      , "  FOREIGN KEY (productId) REFERENCES Product(sku)"
      ]
    constraints "Payment" cols =
      [ "  PRIMARY KEY (paymentId)"
      , "  FOREIGN KEY (orderId) REFERENCES \"Order\"(orderId)"
      ]
    constraints "Coupon" cols =
      [ "  PRIMARY KEY (couponCode)"
      ]
    constraints "Category" cols =
      [ "  PRIMARY KEY (categoryId)"
      ]
    constraints "ProductCategory" cols =
      [ "  PRIMARY KEY (productId, categoryId)"
      , "  FOREIGN KEY (productId) REFERENCES Product(sku)"
      , "  FOREIGN KEY (categoryId) REFERENCES Category(categoryId)"
      ]
    constraints _ _ = []

generateSeed :: [(String, [(String, Name)])] -> String
generateSeed = concatMap tableSeed
  where
    tableSeed ("User", _) = unlines
      [ "INSERT INTO User VALUES (1, 'Arjun', 'arjun@example.com', 30, TRUE);"
      , "INSERT INTO User VALUES (2, 'Priya', 'priya@example.com', 28, TRUE);"
      , "INSERT INTO User VALUES (3, 'Ravi', 'ravi@example.com', 35, FALSE);"
      , "INSERT INTO User VALUES (4, 'Anjali', 'anjali@example.com', 25, TRUE);"
      , "INSERT INTO User VALUES (5, 'Vikram', 'vikram@example.com', 40, TRUE);"
      ]

    tableSeed ("Product", _) = unlines
      [ "INSERT INTO Product VALUES ('SKU001', 'Chai', 'Premium Indian tea', 150.0, 'beverage');"
      , "INSERT INTO Product VALUES ('SKU002', 'Masala', 'Mixed spices', 75.0, 'grocery');"
      , "INSERT INTO Product VALUES ('SKU003', 'Kurkure', 'Spicy snack', 20.0, 'snack');"
      , "INSERT INTO Product VALUES ('SKU004', 'Basmati Rice', 'Long grain rice', 90.0, 'grain');"
      , "INSERT INTO Product VALUES ('SKU005', 'Pickle', 'Mango pickle jar', 60.0, 'condiment');"
      ]

    tableSeed ("Order", _) = unlines
      [ "INSERT INTO \"Order\" VALUES (1001, 1, 230.0, 'Delivered');"
      , "INSERT INTO \"Order\" VALUES (1002, 2, 150.0, 'Processing');"
      , "INSERT INTO \"Order\" VALUES (1003, 3, 75.0, 'Shipped');"
      , "INSERT INTO \"Order\" VALUES (1004, 1, 120.0, 'Cancelled');"
      , "INSERT INTO \"Order\" VALUES (1005, 5, 200.0, 'Delivered');"
      ]

    tableSeed ("Review", _) = unlines
      [ "INSERT INTO Review VALUES (1, 1, 'SKU001', 5, 'Amazing tea!');"
      , "INSERT INTO Review VALUES (2, 2, 'SKU002', 4, 'Loved the spices');"
      , "INSERT INTO Review VALUES (3, 3, 'SKU003', 3, 'Too spicy for me');"
      , "INSERT INTO Review VALUES (4, 4, 'SKU004', 5, 'Best rice I ever bought');"
      , "INSERT INTO Review VALUES (5, 5, 'SKU005', 4, 'Nice and tangy');"
      ]

    tableSeed ("Address", _) = unlines
      [ "INSERT INTO Address VALUES (1, 1, '12 MG Road', 'Near Mall', 'Mumbai', 'MH', '400001');"
      , "INSERT INTO Address VALUES (2, 2, '45 Ring Road', '', 'Delhi', 'DL', '110001');"
      , "INSERT INTO Address VALUES (3, 3, '89 Residency Rd', 'Apt 5B', 'Bangalore', 'KA', '560025');"
      , "INSERT INTO Address VALUES (4, 4, '78 Canal Street', '3rd Floor', 'Kolkata', 'WB', '700091');"
      , "INSERT INTO Address VALUES (5, 5, '21 Nungambakkam', '', 'Chennai', 'TN', '600034');"
      ]

    tableSeed ("Inventory", _) = unlines
      [ "INSERT INTO Inventory VALUES ('SKU001', 100, 'Mumbai');"
      , "INSERT INTO Inventory VALUES ('SKU002', 50, 'Delhi');"
      , "INSERT INTO Inventory VALUES ('SKU003', 200, 'Bangalore');"
      , "INSERT INTO Inventory VALUES ('SKU004', 150, 'Chennai');"
      , "INSERT INTO Inventory VALUES ('SKU005', 90, 'Hyderabad');"
      ]

    tableSeed ("Payment", _) = unlines
      [ "INSERT INTO Payment VALUES (501, 1001, 230.0, 'UPI', 'Completed');"
      , "INSERT INTO Payment VALUES (502, 1002, 150.0, 'Credit Card', 'Pending');"
      , "INSERT INTO Payment VALUES (503, 1003, 75.0, 'NetBanking', 'Completed');"
      , "INSERT INTO Payment VALUES (504, 1004, 120.0, 'UPI', 'Failed');"
      , "INSERT INTO Payment VALUES (505, 1005, 200.0, 'Cash on Delivery', 'Completed');"
      ]

    tableSeed ("Coupon", _) = unlines
      [ "INSERT INTO Coupon VALUES ('NEWUSER10', 10.0, TRUE);"
      , "INSERT INTO Coupon VALUES ('SUMMER15', 15.0, TRUE);"
      , "INSERT INTO Coupon VALUES ('FESTIVE20', 20.0, FALSE);"
      , "INSERT INTO Coupon VALUES ('SUPER25', 25.0, TRUE);"
      , "INSERT INTO Coupon VALUES ('FLAT30', 30.0, FALSE);"
      ]

    tableSeed ("Category", _) = unlines
      [ "INSERT INTO Category VALUES (1, 'Beverages', 'Drinks like tea, coffee etc.');"
      , "INSERT INTO Category VALUES (2, 'Spices', 'Indian spices');"
      , "INSERT INTO Category VALUES (3, 'Snacks', 'Crunchy Indian snacks');"
      , "INSERT INTO Category VALUES (4, 'Grains', 'Rice, Wheat etc.');"
      , "INSERT INTO Category VALUES (5, 'Condiments', 'Pickles and sauces');"
      ]

    tableSeed ("ProductCategory", _) = unlines
      [ "INSERT INTO ProductCategory VALUES ('SKU001', 1);"
      , "INSERT INTO ProductCategory VALUES ('SKU002', 2);"
      , "INSERT INTO ProductCategory VALUES ('SKU003', 3);"
      , "INSERT INTO ProductCategory VALUES ('SKU004', 4);"
      , "INSERT INTO ProductCategory VALUES ('SKU005', 5);"
      ]

    tableSeed _ = ""
