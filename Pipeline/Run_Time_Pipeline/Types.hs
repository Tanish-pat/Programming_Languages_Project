module Types where

type Snippet = String

data Pipeline = Pipeline {
    name :: String,
    steps :: [Snippet]
} deriving (Show, Read)