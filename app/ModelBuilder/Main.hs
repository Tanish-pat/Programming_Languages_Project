{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module ModelBuilder.Main where

import GHC.Generics (Generic)
import Language.Haskell.TH (Name)
import ModelRegistry (models)
import ModelGen (generateAllModels, writeModelToFile)
import qualified Data.Map.Strict as Map

$(generateAllModels models)

main :: IO ()
main = do
  putStrLn "Generating Haskell files for models into the generated/models folder..."
  mapM_ (\(modelName, fields) -> writeModelToFile modelName fields) models
  putStrLn "Model generation complete."