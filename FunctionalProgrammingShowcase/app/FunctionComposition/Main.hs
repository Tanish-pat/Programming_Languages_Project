module Main where

import FunctionComposition
  ( transformNumber
  , applyStringTransformations
  , availableNumFuncs
  , composeAll
  )
import Data.Maybe (mapMaybe)

main :: IO ()
main = cliLoop

cliLoop :: IO ()
cliLoop = do
  putStrLn "\nChoose an option:"
  putStrLn "1. Square, then increment and then double the number"
  putStrLn "2. Apply uppercase+reverse to a string"
  putStrLn "3. Compose functions dynamically"
  putStrLn "Type 'exit' to quit."
  putStr "> "
  choice <- getLine
  case choice of
    "exit" -> do
      putStrLn "Exiting. Goodbye."
      pure ()

    "1" -> do
      putStrLn "Enter a number:"
      x <- readLn
      print (transformNumber x)
      cliLoop

    "2" -> do
      putStrLn "Enter a string:"
      str <- getLine
      putStrLn (applyStringTransformations str)
      cliLoop

    "3" -> do
      putStrLn "Available functions: double, square, increment"
      putStrLn "Enter names to compose (space-separated):"
      line <- getLine
      let names = words line
          funcs = mapMaybe (`lookup` availableNumFuncs) names
      if null funcs
        then putStrLn "No valid functions selected."
        else do
          let composed = composeAll funcs
          putStrLn "Enter a number:"
          n <- readLn
          putStrLn $ "Result: " ++ show (composed n)
      cliLoop

    _ -> do
      putStrLn "Invalid option. Try again."
      cliLoop












-- module Main where

-- import FunctionComposition
--   ( transformNumber
--   , applyStringTransformations
--   , availableNumFuncs
--   , composeAll
--   )
-- import Data.Maybe (mapMaybe)
-- import Text.Read (readMaybe)

-- main :: IO ()
-- main = do
--   putStrLn "1. Square, then increment and then square the number"
--   putStrLn "2. Apply uppercase+reverse to a string"
--   putStrLn "3. Compose functions dynamically"
--   choice <- getLine
--   case choice of
--     "1" -> do
--       putStrLn "Enter a number:"
--       x <- readLn
--       print (transformNumber x)

--     "2" -> do
--       putStrLn "Enter a string:"
--       str <- getLine
--       putStrLn (applyStringTransformations str)

--     "3" -> do
--       putStrLn "Available functions: double, square, increment"
--       putStrLn "Enter names to compose (space-separated):"
--       line <- getLine
--       let names = words line
--           funcs = mapMaybe (`lookup` availableNumFuncs) names
--       if null funcs
--         then putStrLn "No valid functions selected."
--         else do
--           let composed = composeAll funcs
--           putStrLn "Enter a number:"
--           n <- readLn
--           putStrLn $ "Result: " ++ show (composed n)

--     _ -> putStrLn "Invalid option"
