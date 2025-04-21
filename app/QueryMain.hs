{-# LANGUAGE OverloadedStrings #-}
module Main where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.Internal (Field(Field))
import Data.String (fromString)
import QueryPrompter
import System.Console.ANSI
import Control.Monad (unless)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf (printf)

main :: IO ()
main = do
  -- Centered Welcome Banner
  setSGR [SetColor Foreground Vivid Green]
  putStrLn =<< center "  ======================================"
  putStrLn =<< center "   WELCOME TO ROOT CLI INVENTORY SYSTEM"
  putStrLn =<< center "     [ ADMIN SIDE ]"
  putStrLn =<< center "  ======================================"
  setSGR [Reset]

  conn <- open "database/INVENTORY.db"
  loop conn
  close conn

loop :: Connection -> IO ()
loop conn = do
  tbl <- promptTable
  op  <- promptOperation
  kvs <- promptValues tbl op

  let sql = generateSQL tbl op kvs
  setSGR [SetColor Foreground Vivid Yellow]
  putStrLn $ "\n> " ++ sql
  setSGR [Reset]

  case op of
    GetAll   -> queryAndPrint conn sql
    GetByID  -> queryAndPrint conn sql
    _        -> execute_ conn (Query $ fromString sql) >> putStrLn "✔ OK"

  setSGR [SetColor Foreground Vivid Magenta]
  putStrLn "\nDo another? (y/n)"
  setSGR [Reset]
  again <- getLine
  unless (again `elem` ["n","N"]) (loop conn)

-- | run a SELECT and dump each row nicely
queryAndPrint :: Connection -> String -> IO ()
queryAndPrint conn sql = do
  rows <- query_ conn (Query $ fromString sql) :: IO [[SQLData]]
  mapM_ (putStrLn . formatRow) rows

-- | cleanly format a single row of SQLData with types and aligned spacing
formatRow :: [SQLData] -> String
formatRow fields =
  let render val = case val of
        SQLInteger i -> padRight 25 (show i ++ " (Int)")
        SQLFloat f   -> padRight 25 (show f ++ " (Float)")
        SQLText t    -> padRight 25 (T.unpack t ++ " (Text)")
        SQLBlob b    -> padRight 25 (show b ++ " (Blob)")
        SQLNull      -> padRight 25 "NULL (Null)"
  in "  " ++ unwords (map render fields)

-- | pad a string to fixed width
padRight :: Int -> String -> String
padRight n s = s ++ replicate (n - length s) ' '

-- | Center-align text in a 45-character terminal
center :: String -> IO String
center str = do
  let width = 80 -- Use full width of most terminals
      pad = replicate ((width - length str) `div` 2) ' '
  return $ pad ++ str
