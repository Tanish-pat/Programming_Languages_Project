{-# LANGUAGE TemplateHaskell #-}

module Main where

import ModelGen
import Registry
import qualified Data.Map.Strict as Map

-- Generate a model: User with id, name
$(generateModel "User" [("id", ''Int), ("name", ''String)])

-- Generate another model: Product
$(generateModel "Product" [("sku", ''String), ("price", ''Double)])

main :: IO ()
main = do
  let u1 = User 1 "Alice"
  let u2 = User 2 "Bob"

  let p1 = Product "ABC123" 9.99
  let p2 = Product "XYZ999" 19.95

  -- Simulate a dynamic database
  let emptyStore = Map.empty
  let store1 = insertRecord "User" u1 emptyStore
  let store2 = insertRecord "User" u2 store1
  let store3 = insertRecord "Product" p1 store2
  let store4 = insertRecord "Product" p2 store3

  putStrLn "\n--- All Records ---"
  dumpRegistry store4

  putStrLn "\n--- Users Only ---"
  mapM_ putStrLn (getRecords "User" store4)

  putStrLn "\n--- Users with id == 2 ---"
  let typedUsers = getTypedByKey "User" store4
  let filteredUsers = filter (\u -> userId u == 2) typedUsers
  mapM_ print filteredUsers