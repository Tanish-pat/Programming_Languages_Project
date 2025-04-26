module Main where

import MonadLazyEvaluation
import Control.Monad (forever)
import System.Exit (exitSuccess)

main :: IO ()
main = forever $ do
  putStrLn "\nSelect an option:"
  putStrLn "1. Safe Chain Division"
  putStrLn "2. Lazy Infinite List (take N)"
  putStrLn "3. Apply Dynamic Integer Operations"
  putStrLn "4. Apply Dynamic String Operations"
  putStrLn "5. Lazy Conditional Evaluation"
  putStrLn "6. Exit"
  putStr "Choice: "
  choice <- getLine
  case choice of
    "1" -> do
      putStrLn "Enter initial number:"
      x <- readLn
      putStrLn "Enter divisors separated by space:"
      divisors <- fmap (map read . words) getLine
      case chainDivisions x divisors of
        Just result -> putStrLn $ "Result: " ++ show result
        Nothing     -> putStrLn "Division by zero occurred."

    "2" -> do
      putStrLn "How many elements to take?"
      n <- readLn
      print (takeNFromInfinite n)

    "3" -> do
      putStrLn "Available operations: double, square, increment, decrement"
      putStrLn "Enter operations separated by space (applied left to right):"
      ops <- fmap words getLine
      putStrLn "Enter the number to operate on:"
      num <- readLn
      case applyRuntimeOperations ops num of
        Just result -> putStrLn $ "Result: " ++ show result
        Nothing     -> putStrLn "Invalid operation name."

    "4" -> do
      putStrLn "Available string operations: reverse, uppercase, append-exclamation"
      putStrLn "Enter operations separated by space (applied left to right):"
      ops <- fmap words getLine
      putStrLn "Enter the string to operate on:"
      str <- getLine
      case applyRuntimeStringOperations ops str of
        Just result -> putStrLn $ "Result: " ++ result
        Nothing     -> putStrLn "Invalid operation name."

    "5" -> do
      putStrLn "Enter a boolean condition (True/False):"
      condStr <- getLine
      let cond = case condStr of
                   "True"  -> True
                   "False" -> False
                   _       -> False
      putStrLn "Enter true branch value (string):"
      trueBranch <- getLine
      putStrLn "Enter false branch value (string):"
      falseBranch <- getLine
      putStrLn $ "Result: " ++ lazyConditional cond trueBranch falseBranch

    "6" -> do
      putStrLn "Exiting."
      exitSuccess

    _ -> putStrLn "Invalid choice. Please select again."







-- module Main where

-- import MonadLazyEvaluation
--   ( safeDivide
--   , chainDivisions
--   , takeNFromInfinite
--   )

-- main :: IO ()
-- main = cliLoop

-- cliLoop :: IO ()
-- cliLoop = do
--   putStrLn "\nChoose an option:"
--   putStrLn "1. Perform safe chained divisions"
--   putStrLn "2. Generate N numbers from an infinite list"
--   putStrLn "Type 'exit' to quit."
--   putStr "> "
--   choice <- getLine
--   case choice of
--     "exit" -> putStrLn "Exiting. Goodbye."

--     "1" -> do
--       putStrLn "Enter initial number:"
--       start <- readLn
--       putStrLn "Enter divisors separated by space:"
--       divisorsLine <- getLine
--       let divisors = map read (words divisorsLine)
--       case chainDivisions start divisors of
--         Just result -> putStrLn $ "Result: " ++ show result
--         Nothing     -> putStrLn "Division by zero encountered."
--       cliLoop

--     "2" -> do
--       putStrLn "How many numbers to generate?"
--       n <- readLn
--       print (takeNFromInfinite n)
--       cliLoop

--     _ -> do
--       putStrLn "Invalid option. Try again."
--       cliLoop
