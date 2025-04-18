{-# LANGUAGE TemplateHaskell #-}

module Main where

import Language.Haskell.TH (Name)
import ModelRegistry (models)
import ModelGen (generateAllModels, writeModelToFile)
import Registry
import qualified Data.Map.Strict as Map


-- This is now legal because models is imported
$(generateAllModels models)

main :: IO ()
main = do
  putStrLn "Generating Haskell files for models into the generated/models folder..."
  mapM_ (\(modelName, fields) -> writeModelToFile modelName fields) models
  putStrLn "Model generation complete."
