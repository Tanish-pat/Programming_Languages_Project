{-# LANGUAGE OverloadedStrings #-}

module Main where

import DBBuilder (buildSQLiteDB)

main :: IO ()
main = do
    putStrLn "🚧 Building the AMAZON SQLite database..."
    buildSQLiteDB
