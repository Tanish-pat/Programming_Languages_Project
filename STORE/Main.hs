{-# LANGUAGE TemplateHaskell #-}

module Main where

import ModelGen (generateModel, writeModelToFile)
import Registry
import qualified Data.Map.Strict as Map

-- Generate model: User with id, name
$(generateModel "User" [("id", ''Int), ("name", ''String)])

-- Generate model: Product with sku, price
$(generateModel "Product" [("sku", ''String), ("price", ''Double)])

-- Generate model: Order with orderId, userId, total
$(generateModel "Order" [("orderId", ''Int), ("userId", ''Int), ("total", ''Double)])

main :: IO ()
main = do
  -- Generate Haskell files for the models
  putStrLn "Generating Haskell files for User, Product, and Order models..."
  writeModelToFile "User" "User model generated"
  writeModelToFile "Product" "Product model generated"
  writeModelToFile "Order" "Order model generated"

  -- Create sample records
  let u1 = User 1 "Alice"
      u2 = User 2 "Bob"
      u3 = User 3 "Charlie"

  let p1 = Product "ABC123" 9.99
      p2 = Product "XYZ999" 19.95
      p3 = Product "PQR456" 29.95

  let o1 = Order 101 1 29.94  -- Alice's order
      o2 = Order 102 2 19.95  -- Bob's order
      o3 = Order 103 1 59.90  -- Alice's order

  -- Insert records into the registry
  let emptyStore = Map.empty
  let store1 = insertRecord "User" u1 emptyStore
  let store2 = insertRecord "User" u2 store1
  let store3 = insertRecord "User" u3 store2
  let store4 = insertRecord "Product" p1 store3
  let store5 = insertRecord "Product" p2 store4
  let store6 = insertRecord "Product" p3 store5
  let store7 = insertRecord "Order" o1 store6
  let store8 = insertRecord "Order" o2 store7
  let store9 = insertRecord "Order" o3 store8

  -- Dump all records
  putStrLn "\n--- All Records in Registry ---"
  dumpRegistry store9

  -- Retrieve and print all Users
  putStrLn "\n--- All Users ---"
  mapM_ putStrLn (getRecords "User" store9)

  -- Retrieve and print all Products
  putStrLn "\n--- All Products ---"
  mapM_ putStrLn (getRecords "Product" store9)

  -- Retrieve and print all Orders
  putStrLn "\n--- All Orders ---"
  mapM_ putStrLn (getRecords "Order" store9)

  -- Filter Users by name
  putStrLn "\n--- Users named Alice ---"
  let typedUsers = getTypedByKey "User" store9
  let aliceUsers = filter (\u -> userName u == "Alice") typedUsers
  mapM_ print aliceUsers

  -- Filter Products by price > 20
  putStrLn "\n--- Products with price > 20 ---"
  let typedProducts = getTypedByKey "Product" store9
  let expensiveProducts = filter (\p -> productPrice p > 20) typedProducts
  mapM_ print expensiveProducts

  -- Filter Orders by userId == 1 (Alice's orders)
  putStrLn "\n--- Orders for User ID 1 (Alice) ---"
  let typedOrders = getTypedByKey "Order" store9
  let aliceOrders = filter (\o -> orderUserId o == 1) typedOrders
  mapM_ print aliceOrders