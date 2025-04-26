module PureFunctionsCurrying where

import qualified Data.Map as M

-- Pure functions
add :: Int -> Int -> Int
add x y = x + y

multiply :: Int -> Int -> Int
multiply x y = x * y

subtract' :: Int -> Int -> Int
subtract' x y = x - y

divide :: Int -> Int -> Int
divide x y = x `div` y

-- Curried versions
addCurried :: Int -> (Int -> Int)
addCurried = add

multiplyCurried :: Int -> (Int -> Int)
multiplyCurried = multiply

subtractCurried :: Int -> (Int -> Int)
subtractCurried = subtract'

divideCurried :: Int -> (Int -> Int)
divideCurried = divide

-- Runtime dispatch map
functionMap :: M.Map String (Int -> Int -> Int)
functionMap = M.fromList
  [ ("add", add)
  , ("multiply", multiply)
  , ("subtract", subtract')
  , ("divide", divide)
  ]

-- Compose a chain of functions dynamically
composeCurried :: [Int -> Int] -> Int -> Int
composeCurried fs = foldl (flip (.)) id fs
