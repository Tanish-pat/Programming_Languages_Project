-- app/TesterMain.hs
module Main where

import DynamicSQLPrompt

main :: IO ()
main = do
  tables <- parseSchema "database/schema.sql"
  mainLoop tables
