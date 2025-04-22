{-# LANGUAGE OverloadedStrings #-}
module Main where

import Database.SQLite.Simple
import System.Directory (doesFileExist, removeFile)
import System.IO (hFlush, stdout)
import Control.Monad (when)
import Data.String (fromString)
import System.IO (writeFile)

main :: IO ()
main = do
  let dbPath     = "database/INVENTORY.db"
  let schemaPath = "database/schema.sql"
  let seedPath   = "database/seed.sql"

  putStrLn "\n📦  Use existing database or reset?"
  putStrLn "1. Use existing"
  putStrLn "2. Reset database"
  putStr "Enter choice (1/2): "
  hFlush stdout
  choice <- getLine

  case choice of
    "2" -> do
      writeFile ".db-should-reset" "RESET"
      putStrLn "🗑️  Marked database for reset."
    _ -> do
      writeFile ".db-should-reset" "USE"
      putStrLn "👍 Keeping existing database."
