module Main where

import System.Directory (createDirectoryIfMissing)
import ModelGen (writeModelToFile)
import ModelRegistry (models)
import RouteGen (writeModule)
import RouteRegistry (routeRegistry)

main :: IO ()
main = do
  -- Ensure the base output directory exists
  createDirectoryIfMissing True "generated"
  createDirectoryIfMissing True "generated/routes"
  createDirectoryIfMissing True "generated/models"

  -- Model generation
  putStrLn "Generating Haskell files for models into the generated/models folder..."
  mapM_ (\(modelName, fields) -> writeModelToFile "generated" modelName fields) models
  putStrLn "Model generation complete."

  -- Route generation
  putStrLn "Generating routes..."
  mapM_ (writeModule "generated/routes") routeRegistry
  putStrLn "Route generation complete."
