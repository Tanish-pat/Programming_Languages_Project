module Main where

import Data.Dynamic
import FirstClassFunctions

main :: IO ()
main = do
    -- addition
    let add0 :: Int -> Int;  add0 x = x + 10
        addFunc = toDyn add0
        value   = toDyn (5 :: Int)

    case applyFunction addFunc value of
      Just r  -> print (fromDynamic r :: Maybe Int)
      Nothing -> putStrLn "applyFunction failed"

    -- string concatenation
    let cat0 :: String -> String; cat0 s = s ++ " world"
        concatFunc = toDyn cat0
        strVal     = toDyn "Hello"

    case concatDynamic concatFunc strVal of
      Just r  -> print (fromDynamic r :: Maybe String)
      Nothing -> putStrLn "concatDynamic failed"





-- module Main where

-- import FirstClassFunctions

-- -- Main program where functions can be applied dynamically
-- main :: IO ()
-- main = do
--     putStrLn "Choose an option:"
--     putStrLn "1. Apply function to two numbers"
--     putStrLn "2. Create a multiplier and apply it"
--     putStrLn "3. Create a dynamic adder"
--     putStrLn "4. Map a function to a list"
--     putStrLn "5. Compose two functions"

--     choice <- getLine
--     case choice of
--         "1" -> do
--             putStrLn "Enter two numbers:"
--             x <- readLn
--             y <- readLn
--             let result = applyFunction (+) x y
--             putStrLn $ "Result: " ++ show result

--         "2" -> do
--             putStrLn "Enter a number to create a multiplier:"
--             x <- readLn
--             let multiplierFunc = multiplier x
--             putStrLn "Enter a value to multiply:"
--             y <- readLn
--             putStrLn $ "Result: " ++ show (multiplierFunc y)

--         "3" -> do
--             putStrLn "Enter a number to create an adder:"
--             x <- readLn
--             let adder = createAdder x
--             putStrLn "Enter a value to add:"
--             y <- readLn
--             putStrLn $ "Result: " ++ show (adder y)

--         "4" -> do
--             putStrLn "Enter a list of numbers separated by commas:"
--             input <- getLine
--             let numbers = map read (wordsWhen (== ',') input) :: [Int]
--             putStrLn "Enter a function to apply (1: double, 2: square):"
--             op <- getLine
--             let result = case op of
--                             "1" -> mapWith (*2) numbers
--                             "2" -> mapWith (^2) numbers
--                             _   -> numbers
--             putStrLn $ "Result: " ++ show result

--         "5" -> do
--             putStrLn "Enter a number to create a multiplier:"
--             x <- readLn
--             let multiplierFunc = multiplier x
--             let composedFunc = composeFunctions (show . (+1)) (multiplierFunc)
--             putStrLn "Enter a value to apply composed function:"
--             y <- readLn
--             putStrLn $ "Result: " ++ composedFunc y

--         _ -> putStrLn "Invalid choice!"


-- -- Helper function to split strings
-- wordsWhen :: (Char -> Bool) -> String -> [String]
-- wordsWhen p s =  case dropWhile p s of
--     "" -> []
--     s' -> w : wordsWhen p s''
--           where (w, s'') = break p s'
