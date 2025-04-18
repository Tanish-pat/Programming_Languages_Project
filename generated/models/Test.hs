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
    let user = User 1 "John Doe" "john.doe@example.com" 30 True
    let myProduct = Product "SKU123" "Gadget" "Cool gadget" 199.99 "tech"
    let order = Order 1 1 199.99 "shipped"
    let review = Review 1 1 "SKU123" 5 "Great product!"
    let address = Address 1 1 "123 Main St" "Apt 4B" "New York" "NY" "10001"
    let inventory = Inventory "SKU123" 100 "Warehouse A"
    let payment = Payment 1 1 199.99 "Credit Card" "Completed"
    let coupon = Coupon "DISCOUNT10" 10.0 True
    let category = Category 1 "Electronics" "Electronic gadgets"
    let productCategory = ProductCategory "SKU123" 1

    -- Print all the test data to verify they are created
    print user
    print myProduct
    print order
    print review
    print address
    print inventory
    print payment
    print coupon
    print category
    print productCategory

    putStrLn "Test data created and printed successfully!"
