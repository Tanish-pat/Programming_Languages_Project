{-# LANGUAGE OverloadedStrings #-}

module DBBuilder where

import Database.SQLite.Simple
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import System.FilePath ((</>))
import Control.Monad (forM_, when)
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- | Split SQL text at semicolons (SQLite needs individual statements)
splitSQL :: T.Text -> [T.Text]
splitSQL = filter (not . T.null . T.strip) . T.splitOn ";"

-- | Builds and populates the SQLite database
buildSQLiteDB :: IO ()
buildSQLiteDB = do
    let dir = "database"
    let dbPath = dir </> "INVENTORY.db"
    let schemaPath = dir </> "schema.sql"
    let seedPath = dir </> "seed.sql"

    createDirectoryIfMissing True dir

    dbExists <- doesFileExist dbPath
    when dbExists $ do
        putStrLn "💣 Removing existing INVENTORY.db to avoid constraint errors..."
        removeFile dbPath

    conn <- open dbPath

    -- Step 1: Run schema.sql
    putStrLn "🛠️  Creating tables from schema.sql..."
    schemaText <- T.readFile schemaPath
    forM_ (splitSQL schemaText) $ \stmt ->
        execute_ conn (Query $ T.strip stmt)

    -- Step 2: Run seed.sql
    putStrLn "🌱 Inserting data from seed.sql..."
    seedText <- T.readFile seedPath
    forM_ (splitSQL seedText) $ \stmt ->
        execute_ conn (Query $ T.strip stmt)

    close conn
    putStrLn $ "✅ Done! Database created at: " ++ dbPath
