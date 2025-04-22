{-# LANGUAGE OverloadedStrings #-}
module SchemaParser (parseSchemaSQL) where

import qualified Data.Text as T
import qualified Data.Map as M
import System.IO

type ColumnTypeMap = M.Map String (M.Map String String)
type PrimaryKeyMap = M.Map String [String]

-- | Entry point: returns (table -> column:type map, table -> primary key list)
parseSchemaSQL :: FilePath -> IO (ColumnTypeMap, PrimaryKeyMap)
parseSchemaSQL path = do
  raw <- readFile path
  let blocks = splitTables (T.pack raw)
      parsed = mapMaybeSafe parseTable blocks
      (colMap, pkMap) = unzip parsed
  return (M.fromList colMap, M.fromList pkMap)

-- | Split raw schema by CREATE TABLE
splitTables :: T.Text -> [T.Text]
splitTables = filter (not . T.null) . map T.strip . T.splitOn "CREATE TABLE"

-- | Parse a full CREATE TABLE block
parseTable :: T.Text -> Maybe ((String, M.Map String String), (String, [String]))
parseTable block =
  let ls = map T.strip $ T.lines block
  in case ls of
       [] -> Nothing
       (header:rest) ->
         case extractTableName header of
           Nothing   -> Nothing
           Just name ->
             let colDefs = filter (not . isConstraint) rest
                 pkLines = filter ("PRIMARY KEY" `T.isInfixOf`) rest
                 cols    = M.fromList $ mapMaybeSafe parseColumn colDefs
                 pks     = concatMap parsePrimaryKey pkLines
             in Just ((name, cols), (name, pks))

-- | Extract table name from the first line
extractTableName :: T.Text -> Maybe String
extractTableName line =
  let ws = words $ T.unpack line
  in case ws of
       ("CREATE":"TABLE":"IF":"NOT":"EXISTS":name:_) -> Just (clean name)
       ("IF":"NOT":"EXISTS":name:_)                 -> Just (clean name)
       ("TABLE":name:_)                             -> Just (clean name)
       (_ : "TABLE" : name : _)                     -> Just (clean name)
       _                                            -> Nothing
  where clean = filter (`notElem` ("\"`(){};" :: String))

-- | Extract column name and type
parseColumn :: T.Text -> Maybe (String, String)
parseColumn line =
  let parts = words (T.unpack line)
  in case parts of
       (name:typ:_) -> Just (clean name, clean typ)
       _            -> Nothing
  where clean = filter (`notElem` ("\",()" :: String))

-- | Parse PRIMARY KEY (...) line into a list of columns
parsePrimaryKey :: T.Text -> [String]
parsePrimaryKey line =
  let inside = takeWhile (/= ')') . drop 1 . dropWhile (/= '(') $ T.unpack line
  in map (filter (`notElem` ("\"` " :: String))) $ splitCommas inside
  where splitCommas = map T.unpack . T.splitOn "," . T.pack

-- | Recognize constraint lines like PRIMARY KEY or FOREIGN KEY
isConstraint :: T.Text -> Bool
isConstraint line =
  any (`T.isInfixOf` line)
    ["PRIMARY KEY", "FOREIGN KEY", "UNIQUE", "CHECK", "REFERENCES", ");"]

-- | Safe version of mapMaybe for better logging/debugging
mapMaybeSafe :: (a -> Maybe b) -> [a] -> [b]
mapMaybeSafe f = foldr (\x acc -> maybe acc (:acc) (f x)) []
