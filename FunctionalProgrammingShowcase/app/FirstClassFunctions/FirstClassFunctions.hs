module FirstClassFunctions where

import Data.Dynamic
import Data.Maybe (isJust)

-- 1. Dynamic Function Invocation: addition
addDynamic :: Dynamic -> Dynamic -> Maybe Dynamic
addDynamic x y = do
    x' <- fromDynamic x :: Maybe Int
    y' <- fromDynamic y :: Maybe Int
    return $ toDyn (x' + y')

-- 2. String Concatenation with Dynamic Types
concatDynamic :: Dynamic -> Dynamic -> Maybe Dynamic
concatDynamic x y = do
    x' <- fromDynamic x :: Maybe String
    y' <- fromDynamic y :: Maybe String
    return $ toDyn (x' ++ y')

-- 3. Runtime Type Checking
checkType :: Dynamic -> String
checkType x
    | isJust (fromDynamic x :: Maybe Int)    = "Int"
    | isJust (fromDynamic x :: Maybe String) = "String"
    | otherwise                              = "Unknown type"

-- 4. Apply Function Dynamically (Int -> Int)
applyFunction :: Dynamic -> Dynamic -> Maybe Dynamic
applyFunction f x = do
    f' <- fromDynamic f :: Maybe (Int -> Int)
    x' <- fromDynamic x :: Maybe Int
    return $ toDyn (f' x')

-- 5. Function Selection Based on Runtime Input
multiplyDynamic :: Dynamic -> Dynamic -> Maybe Dynamic
multiplyDynamic x y = do
    x' <- fromDynamic x :: Maybe Int
    y' <- fromDynamic y :: Maybe Int
    return $ toDyn (x' * y')

chooseFunction :: Dynamic -> Dynamic -> Maybe Dynamic
chooseFunction op x = do
    opStr <- fromDynamic op :: Maybe String
    case opStr of
      "add"      -> addDynamic x (toDyn (5 :: Int))
      "multiply" -> multiplyDynamic x (toDyn (3 :: Int))
      _          -> Nothing










-- module FirstClassFunctions where

-- -- A simple higher-order function that takes a function and applies it to two arguments
-- applyFunction :: (a -> b -> c) -> a -> b -> c
-- applyFunction f x y = f x y

-- -- A function that returns another function (Closure example)
-- multiplier :: Int -> (Int -> Int)
-- multiplier x = (\y -> x * y)

-- -- Function to create a dynamic function at runtime
-- createAdder :: Int -> (Int -> Int)
-- createAdder x = (\y -> x + y)

-- -- Higher order function that accepts a function to manipulate the list of numbers
-- mapWith :: (a -> b) -> [a] -> [b]
-- mapWith f = map f

-- -- Function that allows to pass functions dynamically (useful for runtime decisions)
-- dynamicOperation :: (Int -> Int -> Int) -> Int -> Int -> Int
-- dynamicOperation f x y = f x y

-- -- A higher order function that takes another function as argument and returns a function
-- composeFunctions :: (b -> c) -> (a -> b) -> a -> c
-- composeFunctions f g = f . g
