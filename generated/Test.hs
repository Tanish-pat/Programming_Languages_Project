module Main where

import User
import Product
import Order
import Review
import Address
import Inventory
import Payment
import Coupon
import Category
import ProductCategory

main :: IO ()
main = do
  putStrLn "Running Model Tests..."

  let user = User 1 "Alice" "alice@example.com" 30 True
  let prod = Product "SKU123" "Gadget" "Cool gadget" 199.99 "tech"
  let order = Order 1001 1 199.99 "Processing"
  let review = Review 9001 1 "SKU123" 5 "Excellent product!"
  let address = Address 101 1 "123 Main St" "Apt 4B" "New York" "NY" "10001"
  let inventory = Inventory "SKU123" 50 "Warehouse A"
  let payment = Payment 501 1001 199.99 "Credit Card" "Completed"
  let coupon = Coupon "SUMMER20" 20.0 True
  let category = Category 10 "Electronics" "Devices and gadgets"
  let prodCat = ProductCategory "SKU123" 10

  putStrLn "\nUser:"
  print user

  putStrLn "\nProduct:"
  print prod

  putStrLn "\nOrder:"
  print order

  putStrLn "\nReview:"
  print review

  putStrLn "\nAddress:"
  print address

  putStrLn "\nInventory:"
  print inventory

  putStrLn "\nPayment:"
  print payment

  putStrLn "\nCoupon:"
  print coupon

  putStrLn "\nCategory:"
  print category

  putStrLn "\nProductCategory:"
  print prodCat

  putStrLn "\nAll tests completed."
