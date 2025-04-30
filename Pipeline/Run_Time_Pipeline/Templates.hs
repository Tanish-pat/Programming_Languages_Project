module Templates (templates) where
import Data.Char (toUpper)
import Types

templates :: [(String, Snippet)]
templates =
  [ ("Reverse string", "\\x -> reverse x")
  , ("Uppercase", "\\x -> map toUpper x")
  , ("Add prefix 'Hi: '", "\\x -> \"Hi: \" ++ x")
  , ("Add suffix '!!!'", "\\x -> x ++ \"!!!\"")
  , ("First 5 characters", "\\x -> take 5 x")
  ]