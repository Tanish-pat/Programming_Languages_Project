{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module ModelBuilder.Main where

import GHC.Generics (Generic)
import Language.Haskell.TH (Name)
import ModelRegistry (models)  -- this models is the runtime model data
import ModelGen (generateAllModels, writeModelToFile)
import qualified Data.Map.Strict as Map

-- 👇 Import the schema + seed builder
import SchemaSeedBuilder (genSchemaAndSeed)


-- ✅ Use a different name in TH to avoid conflict
-- ✅ Generate Haskell models + schema/seed.sql at compile time
$(generateAllModels models)
$(genSchemaAndSeed)  

main :: IO ()
main = do
  putStrLn "Generating Haskell files for models into the generated/models folder..."
  mapM_ (\(modelName, fields) -> writeModelToFile modelName fields) models
  putStrLn "Model generation complete."
