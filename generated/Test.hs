module Main where

import User
import Product
import Order

main :: IO ()
main = do
  -- Create some sample data
  let u = User 1 "Alice"
      p = Product "XYZ123" 19.99
      o = Order 1001 1 39.98

  -- Print them to verify
  putStrLn "Testing generated models..."
  print u
  print p
  print o
