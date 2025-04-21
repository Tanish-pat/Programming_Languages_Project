{-# LANGUAGE OverloadedStrings #-}

module DynamicSQLPrompt where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)
import Data.Char (toLower)
import Data.List (isInfixOf)
import System.Console.ANSI
import System.IO (stdout)

data Column = Column
  { colName :: String
  , colType :: String
  } deriving (Show)

data Table = Table
  { tableName :: String
  , tableColumns :: [Column]
  , tableForeignKeys :: [(String, String, String)]
  } deriving (Show)

data Action = Insert | Update | Delete | Select
  deriving (Show, Enum, Bounded)

logFile :: FilePath
logFile = "query_log.sql"

-- Wraps string values with quotes if needed
wrap :: String -> String
wrap v = if all (`elem` (".0123456789" :: String)) v
         then v
         else "'" ++ v ++ "'"

parseSchema :: FilePath -> IO [Table]
parseSchema path = do
  content <- TIO.readFile path
  let statements = T.splitOn ";" content
      createTables = filter containsCreateTable statements
  return $ map parseTable createTables
  where
    containsCreateTable :: T.Text -> Bool
    containsCreateTable text = "CREATE TABLE" `T.isInfixOf` (T.toUpper text)

parseTable :: T.Text -> Table
parseTable stmt =
  let ls = filter (not . T.null) $ map T.strip (T.lines stmt)
      createLine = head [l | l <- ls, "CREATE TABLE" `T.isInfixOf` (T.toUpper l)]
      name = T.unpack $ T.strip $
             T.takeWhile (/= '(') $
             T.dropWhile (== ' ') $
             T.drop (T.length "CREATE TABLE") createLine
      bodyLines = filter (\l -> not ("CREATE TABLE" `T.isInfixOf` (T.toUpper l)) &&
                             not ("PRIMARY KEY" `T.isPrefixOf` l) &&
                             not (")" `T.isPrefixOf` l)) ls
      (cols, foreignKeys) = foldl parseLine ([], []) bodyLines
  in Table name cols foreignKeys

extractTableName :: T.Text -> String
extractTableName line =
  let wordsList = T.words line
      tableWord = last wordsList
  in T.unpack $ T.strip $ T.filter (/= '`') tableWord

parseLine :: ([Column], [(String, String, String)]) -> T.Text -> ([Column], [(String, String, String)])
parseLine (cols, foreignKeys) line
  | "FOREIGN KEY" `T.isPrefixOf` line =
      let tokens = T.words line
          col = T.unpack $ T.filter (/= '(') $ tokens !! 2
          refTable = T.unpack $ T.splitOn "(" (tokens !! 4) !! 0
          refCol = T.unpack $ T.filter (/= ')') $ T.splitOn "(" (tokens !! 4) !! 1
      in (cols, (col, refTable, refCol) : foreignKeys)
  | "PRIMARY KEY" `T.isPrefixOf` line = (cols, foreignKeys)
  | otherwise =
      let tokens = T.words line
          colName = T.unpack $ T.filter (/= '`') $ tokens !! 0
          colType = T.unpack $ tokens !! 1
      in (Column colName colType : cols, foreignKeys)

generateSQL :: Table -> Action -> IO String
generateSQL table action = do
  case action of
    Insert -> do
      putStrLn $ "Enter values for " ++ tableName table ++ ":"
      values <- mapM (\col -> putStr (colName col ++ ": ") >> hFlush stdout >> getLine) (tableColumns table)
      let cols = map colName (tableColumns table)
          valStr = map wrap values
      return $ "INSERT INTO " ++ tableName table ++ " (" ++ commaSep cols ++ ") VALUES (" ++ commaSep valStr ++ ");"

    Update -> do
      putStrLn $ "Enter SET clause values for " ++ tableName table ++ ":"
      values <- mapM (\col -> putStr (colName col ++ ": ") >> hFlush stdout >> getLine) (tableColumns table)
      let assignments = zipWith (\c v -> c ++ " = " ++ wrap v) (map colName $ tableColumns table) values
      putStr "Enter WHERE clause (e.g., id = 1): " >> hFlush stdout
      cond <- getLine
      return $ "UPDATE " ++ tableName table ++ " SET " ++ commaSep assignments ++ " WHERE " ++ cond ++ ";"

    Delete -> do
      putStr   $ "Enter WHERE clause for DELETE on " ++ tableName table ++ ": "
      hFlush stdout
      cond <- getLine
      return $ "DELETE FROM " ++ tableName table ++ " WHERE " ++ cond ++ ";"

    Select -> do
      putStr   $ "Enter WHERE clause (press ENTER for none): "
      hFlush stdout
      cond <- getLine
      return $ "SELECT * FROM " ++ tableName table ++ (if null cond then ";" else " WHERE " ++ cond ++ ";")

commaSep :: [String] -> String
commaSep = foldr1 (\a b -> a ++ ", " ++ b)

mainLoop :: [Table] -> IO ()
mainLoop tables = do
  putStrLn "\nAvailable Tables:"
  let indexedTables = zip [1..] tables
  mapM_ (\(i, t) -> putStrLn $ show i ++ ". " ++ tableName t) indexedTables
  let exitIdx = length tables + 1
  putStrLn $ show exitIdx ++ ". Exit"
  putStr "Select a table number: " >> hFlush stdout
  tableInput <- getLine
  case readMaybe tableInput of
    Just i | i == exitIdx -> putStrLn "Exiting..."
           | i >= 1 && i <= length tables -> do
                let tbl = tables !! (i - 1)
                actionLoop tbl
                mainLoop tables
    _ -> putStrLn "Invalid selection.\n" >> mainLoop tables

actionLoop :: Table -> IO ()
actionLoop tbl = do
  -- Display table structure first
  showTableStructure tbl

  -- Then show actions as before
  putStrLn $ "\nActions for table: " ++ tableName tbl
  let actions = [minBound .. maxBound] :: [Action]
  mapM_ (\(i,a) -> putStrLn $ show i ++ ". " ++ show a) (zip [1..] actions)
  let backIdx = length actions + 1
  putStrLn $ show backIdx ++ ". Back"
  putStr "Select an action number: " >> hFlush stdout
  actionInput <- getLine
  case readMaybe actionInput of
    Just i | i == backIdx -> return ()
           | i >= 1 && i <= length actions -> do
                let act = actions !! (i - 1)
                sql <- generateSQL tbl act
                putStrLn $ "\nGenerated SQL:\n" ++ sql
                appendFile logFile (sql ++ "\n")
                actionLoop tbl
    _ -> putStrLn "Invalid action.\n" >> actionLoop tbl

showTableStructure :: Table -> IO ()
showTableStructure tbl = do
  setSGR [SetColor Foreground Vivid Blue]
  putStrLn $ "\nTable Structure: " ++ tableName tbl
  setSGR [Reset]

  setSGR [SetColor Foreground Vivid Cyan]
  putStrLn "Columns:"
  setSGR [Reset]
  mapM_ (\col -> putStrLn $ "  " ++ colName col ++ " : " ++ colType col) (tableColumns tbl)

  if not (null (tableForeignKeys tbl))
    then do
      setSGR [SetColor Foreground Vivid Magenta]
      putStrLn "\nForeign Keys:"
      setSGR [Reset]
      mapM_ (\(col, refTable, refCol) ->
        putStrLn $ "  " ++ col ++ " → " ++ refTable ++ "(" ++ refCol ++ ")"
        ) (tableForeignKeys tbl)
    else return ()






















