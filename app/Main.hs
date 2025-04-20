module Main where

import qualified ModelBuilder.Main as ModelBuilder (main)
import qualified RoutesBuilder.Main as RoutesBuilder (main)

main :: IO ()
main = do
    putStrLn "Running ModelBuilder (includes schema + seed generation)..."
    ModelBuilder.main
    putStrLn "Running RouteBuilder..."
    RoutesBuilder.main
    putStrLn "All tasks completed!"
