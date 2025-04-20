module Main where

import qualified ModelBuilder.Main as ModelBuilder (main)
import qualified RoutesBuilder.Main as RoutesBuilder (main)

main :: IO ()
main = do
    putStrLn "Running ModelBuilder..."
    ModelBuilder.main
    putStrLn "Running RouteBuilder..."
    RoutesBuilder.main
    putStrLn "All tasks completed!"