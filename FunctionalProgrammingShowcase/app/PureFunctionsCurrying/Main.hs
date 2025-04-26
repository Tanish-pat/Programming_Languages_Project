module Main where

import PureFunctionsCurrying
import qualified Data.Map as M
import Data.Maybe (mapMaybe)

main :: IO ()
main = loop

loop :: IO ()
loop = do
  putStrLn "Available functions: add, multiply, subtract, divide"
  putStrLn "Enter functions to apply (space-separated):"
  funcs <- words <$> getLine

  putStrLn "Enter first input number:"
  x <- readLn

  putStrLn "Enter second input number:"
  y <- readLn

  let operations = mapMaybe (`M.lookup` functionMap) funcs
  if null operations
    then putStrLn "No valid operations selected."
    else do
      let results = map (\op -> op x y) operations
      putStrLn $ "Results: " ++ show results

  putStrLn "Continue? (y/n)"
  cont <- getLine
  if cont == "y" then loop else putStrLn "Goodbye."
