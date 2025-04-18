{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Registry
  ( Storage
  , insertRecord
  , getRecords
  , dumpRegistry
  , getTypedRecords
  , getTypedByKey
  , filterBy
  ) where

import Data.Typeable
import Data.Maybe (mapMaybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- A boxed dynamic object with runtime type info
data AnyModel where
  AnyModel :: (Show a, Typeable a) => a -> AnyModel

type Storage = Map String [AnyModel]

-- Add a record to the registry
insertRecord :: (Show a, Typeable a) => String -> a -> Storage -> Storage
insertRecord modelName record =
  Map.insertWith (++) modelName [AnyModel record]

-- Show all records for a type name (stringly typed)
getRecords :: String -> Storage -> [String]
getRecords modelName registry =
  case Map.lookup modelName registry of
    Just xs -> map (\(AnyModel x) -> show x) xs
    Nothing -> []

-- Dump entire registry (string output)
dumpRegistry :: Storage -> IO ()
dumpRegistry registry =
  mapM_ (\(k, vs) -> do
           putStrLn $ "Model: " ++ k
           mapM_ (\(AnyModel v) -> putStrLn ("  " ++ show v)) vs
        ) (Map.toList registry)

-- Extract all records of a specific type from entire registry
getTypedRecords :: forall a. Typeable a => Storage -> [a]
getTypedRecords store =
  concatMap extractTyped (Map.elems store)
  where
    extractTyped :: [AnyModel] -> [a]
    extractTyped = mapMaybe (\(AnyModel x) -> cast x)

-- Extract typed values only from a specific model name
getTypedByKey :: forall a. Typeable a => String -> Storage -> [a]
getTypedByKey modelName store =
  case Map.lookup modelName store of
    Just models -> mapMaybe (\(AnyModel x) -> cast x) models
    Nothing -> []

-- Filter all records of a given type using a predicate
filterBy :: forall a. Typeable a => (a -> Bool) -> Storage -> [a]
filterBy predicate store = filter predicate (getTypedRecords store)