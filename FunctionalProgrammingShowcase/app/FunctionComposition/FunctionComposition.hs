module FunctionComposition(compose,double,square,increment,reverseString,uppercase,transformNumber,composeAll,applyStringTransformations,availableNumFuncs) where
import Data.Char (toUpper)

-- Function composition operator (.)
-- Composing two functions: f . g = \x -> f (g x)
compose :: (b -> c) -> (a -> b) -> a -> c
compose f g = \x -> f (g x)

-- Example: Simple functions to compose
double :: Int -> Int
double x = x * 2

square :: Int -> Int
square x = x * x

increment :: Int -> Int
increment x = x + 1

reverseString :: String -> String
reverseString = reverse

uppercase :: String -> String
uppercase = map toUpper

-- Function to combine multiple transformations on a number
transformNumber :: Int -> Int
transformNumber = compose double (compose increment square)

-- A higher-order function to compose multiple functions
composeAll :: [a -> a] -> (a -> a)
composeAll = foldl (flip (.)) id

-- Function to create a function that applies multiple string transformations
applyStringTransformations :: String -> String
applyStringTransformations = compose reverseString (compose uppercase id)

-- Lookup table for numeric functions
availableNumFuncs :: [(String, Int -> Int)]
availableNumFuncs =
  [ ("double",    double)
  , ("square",    square)
  , ("increment", increment)
  ]