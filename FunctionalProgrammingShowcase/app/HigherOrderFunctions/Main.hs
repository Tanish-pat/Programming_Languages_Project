module Main where

import Data.Dynamic
import HigherOrderFunctions

main :: IO ()
main = do
    -- prepare typed functions before Dyn
    let f0 :: Int -> Int;   f0 x = x + 1
        g0 :: Int -> Int;   g0 x = x * 2
        f  = toDyn f0
        g  = toDyn g0
        x  = toDyn (3 :: Int)

    case composeDynamic f g x of
      Just res -> print (fromDynamic res :: Maybe Int)
      Nothing  -> putStrLn "composition failed"

    -- prepare map function
    let m0 :: Int -> Int; m0 x = x + 1
        func = toDyn m0
        list = toDyn ([1,2,3] :: [Int])

    case dynamicMap func list of
      Just res -> print (fromDynamic res :: Maybe [Int])
      Nothing  -> putStrLn "map failed"











-- module Main where

-- import HigherOrderFunctions
--   ( applyDynamicOp
--   , applyDynamicStringOp
--   )
-- import Text.Read (readMaybe)

-- data InputType
--   = Number Int
--   | StringInput String
--   | ListOfNumbers [Int]
--   | ListOfStrings [String]
--   deriving Show

-- -- Parse the input string to determine the type of input (Int, String, List of Ints, List of Strings)
-- parseInput :: String -> InputType
-- parseInput s
--   -- Single integer?
--   | Just n <- readMaybe s = Number n

--   -- Comma‐separated list of ints?
--   | ',' `elem` s
--   , let parts = splitComma s
--   , Just nums <- traverse readMaybe parts
--   = ListOfNumbers nums

--   -- Otherwise treat as string or list of strings
--   | otherwise
--   = if ',' `elem` s
--       then ListOfStrings (splitComma s)
--       else StringInput s

-- -- Split a string by commas without spaces
-- splitComma :: String -> [String]
-- splitComma = foldr go [[]]
--   where
--     go ',' acc = [] : acc
--     go c   (y:ys) = (c:y):ys
--     go _   []     = []  -- never hits

-- -- Define the operations for numbers
-- applyOperation :: String -> Int -> Int
-- applyOperation "double" n  = n * 2
-- applyOperation "square" n  = n * n
-- applyOperation _ n         = n  -- No operation

-- -- Loop for continuous CLI interaction
-- loop :: IO ()
-- loop = do
--   putStrLn "\nEnter your input (number, string, or list of numbers/strings) or type 'exit' to quit:"
--   inputRaw <- getLine
--   if inputRaw == "exit"
--     then putStrLn "Goodbye!"
--     else do
--       -- Parsing input
--       let inputType = parseInput inputRaw
--       case inputType of
--         Number n -> do
--           putStrLn "Choose an operation for a number: double | square"
--           operation <- getLine
--           let result = applyOperation operation n
--           putStrLn $ "Result: " ++ show result

--         ListOfNumbers ns -> do
--           putStrLn "Choose an operation for the list of numbers: double | square"
--           operation <- getLine
--           let res = map (applyOperation operation) ns
--           putStrLn $ "Result: " ++ show res

--         StringInput str -> do
--           putStrLn "Choose an operation for the string: reverse | upper | lower"
--           operation <- getLine
--           let [r] = applyDynamicStringOp operation [str]
--           putStrLn $ "Result: " ++ r

--         ListOfStrings ss -> do
--           putStrLn "Choose an operation for the list of strings: reverse | upper | lower"
--           operation <- getLine
--           let res = applyDynamicStringOp operation ss
--           putStrLn $ "Result: " ++ unwords res

--       loop  -- Recursively call loop to keep asking for input

-- -- Main program entry point
-- main :: IO ()
-- main = loop
