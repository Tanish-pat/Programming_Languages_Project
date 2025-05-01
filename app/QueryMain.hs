{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified QueryPrompter as QP

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.Internal (Field(Field))
import Data.String (fromString)
import qualified Data.Map as M
import QueryPrompter
import Control.Exception (SomeException, try)
import System.Console.ANSI
import Control.Monad (unless)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf (printf)
import qualified Data.Map as M
import Data.Map (Map)

main :: IO ()
main = do
  -- ✅ Load table columns dynamically from schema.sql
  tableCols <- loadTableSchema "database/schema.sql"

  -- Centered Welcome Banner
  setSGR [SetColor Foreground Vivid Green]
  putStrLn =<< center "  ======================================"
  putStrLn =<< center "   WELCOME TO ROOT CLI INVENTORY SYSTEM"
  putStrLn =<< center "     [ ADMIN SIDE ]"
  putStrLn =<< center "  ======================================"
  setSGR [Reset]

  putStrLn "\nPress Enter to continue..."
  _ <- getLine
  conn <- open "database/INVENTORY.db"
  loop conn tableCols
  close conn

loop :: Connection -> Map Table [String] -> IO ()
loop conn tableCols = do
  mtbl <- promptTable
  case mtbl of
    Nothing -> do
      setSGR [SetColor Foreground Vivid Yellow]
      putStrLn "\nExiting... Goodbye!"
      setSGR [Reset]
    Just tbl -> do
      mop <- promptOperation
      case mop of
        Nothing -> loop conn tableCols -- "Go Back" option
        Just op -> do
          kvs <- promptValues tbl op
          let sql = generateSQL tbl op kvs

          putStrLn ""
          setSGR [SetColor Foreground Vivid Yellow]
          putStrLn $ "Running SQL: " ++ sql
          setSGR [Reset]

          result <- try $ case op of
            Insert -> do
              execute_ conn (fromString sql)
              putStrLn "✅ Inserted successfully."

            Update -> do
              execute_ conn (fromString sql)
              rows <- changes conn
              if rows == 0
                then putStrLn "❌ Update failed: Record not found."
                else putStrLn "✅ Updated successfully."

            Delete -> do
              execute_ conn (fromString sql)
              rows <- changes conn
              if rows == 0
                then putStrLn "❌ Delete failed: Record not found."
                else putStrLn "✅ Deleted successfully."

            GetByID -> do
              rows <- query_ conn (fromString sql) :: IO [[SQLData]]
              if null rows
                then putStrLn "❌ Record not found."
                else prettyPrintRows tableCols tbl rows

            GetAll -> do
              rows <- query_ conn (fromString sql) :: IO [[SQLData]]
              if null rows
                then putStrLn "ℹ️  No records found."
                else prettyPrintRows tableCols tbl rows

          case result of
            Left err -> do
              setSGR [SetColor Foreground Vivid Red]
              putStrLn $ "❌ Error: " ++ show (err :: SomeException)
              setSGR [Reset]
            Right _ -> return ()

          setSGR [SetColor Foreground Vivid Magenta]
          putStrLn "\nDo another? (y/n)"
          setSGR [Reset]
          again <- getLine
          unless (again `elem` ["n", "N"]) (loop conn tableCols)

--------------------------------------------------------------------------------
-- Utility Functions

prettyPrintRows :: Map Table [String] -> Table -> [[SQLData]] -> IO ()
prettyPrintRows tableCols tbl rows = do
  let cols = tableCols M.! tbl
  mapM_ (printRow cols) rows
  where
    formatValue :: SQLData -> String
    formatValue (SQLInteger i) = show i
    formatValue (SQLFloat f)   = show f
    formatValue (SQLText t)    = T.unpack t
    formatValue SQLNull        = "NULL"
    formatValue val            = show val

    printRow :: [String] -> [SQLData] -> IO ()
    printRow colNames values = do
      let pairs = zip colNames values
          rowStr = unwords [c ++ ": " ++ formatValue v | (c,v) <- pairs]
      putStrLn rowStr

padRight :: Int -> String -> String
padRight n s = s ++ replicate (n - length s) ' '

center :: String -> IO String
center str = do
  let width = 80
      pad = replicate ((width - length str) `div` 2) ' '
  return $ pad ++ str
