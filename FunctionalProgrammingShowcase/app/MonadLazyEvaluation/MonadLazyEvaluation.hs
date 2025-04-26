module MonadLazyEvaluation where

import Control.Monad (when)
import Data.Char (toUpper)

-- Example 1: Maybe Monad for safe computations
safeDivide :: Int -> Int -> Maybe Int
safeDivide _ 0 = Nothing
safeDivide x y = Just (x `div` y)

chainDivisions :: Int -> [Int] -> Maybe Int
chainDivisions = foldl (\acc y -> acc >>= (`safeDivide` y)) . Just

-- Example 2: Lazy evaluation
generateInfiniteList :: [Int]
generateInfiniteList = [1..]

takeNFromInfinite :: Int -> [Int]
takeNFromInfinite n = take n generateInfiniteList

-- Example 3: Dynamic operation execution (Runtime meta-programming)

type IntOperation = Int -> Int

availableOperations :: [(String, IntOperation)]
availableOperations =
  [ ("double", (*2))
  , ("square", \x -> x * x)
  , ("increment", (+1))
  , ("decrement", subtract 1)
  ]

applyRuntimeOperations :: [String] -> Int -> Maybe Int
applyRuntimeOperations ops x = do
  functions <- traverse (`lookup` availableOperations) ops
  return (foldl (\acc f -> f acc) x functions)

-- Example 4: Dynamic String Transformations

type StringOperation = String -> String

stringOperations :: [(String, StringOperation)]
stringOperations =
  [ ("reverse", reverse)
  , ("uppercase", map toUpper)
  , ("append-exclamation", (++ "!"))
  ]

applyRuntimeStringOperations :: [String] -> String -> Maybe String
applyRuntimeStringOperations ops str = do
  funcs <- traverse (`lookup` stringOperations) ops
  return (foldl (\acc f -> f acc) str funcs)

-- Example 5: Lazy conditional evaluation
lazyConditional :: Bool -> a -> a -> a
lazyConditional cond trueBranch falseBranch =
  if cond then trueBranch else falseBranch








-- module MonadLazyEvaluation where

-- -- Example 1: Using Maybe Monad for safe computations
-- safeDivide :: Int -> Int -> Maybe Int
-- safeDivide _ 0 = Nothing
-- safeDivide x y = Just (x `div` y)

-- chainDivisions :: Int -> [Int] -> Maybe Int
-- chainDivisions = foldl (\acc y -> acc >>= (`safeDivide` y)) . Just

-- -- Example 2: Lazy Evaluation in action
-- generateInfiniteList :: [Int]
-- generateInfiniteList = [1..]

-- takeNFromInfinite :: Int -> [Int]
-- takeNFromInfinite n = take n generateInfiniteList
