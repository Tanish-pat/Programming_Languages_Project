{-# LANGUAGE TemplateHaskell #-}

module ModelGen (generateModel, writeModelToFile) where

import Language.Haskell.TH
import Language.Haskell.TH.Ppr (ppr)
import Language.Haskell.TH.PprLib (Doc, text, hcat, vcat, ($+$), empty, punctuate, comma)
import Data.Char (toLower, toUpper)
import Data.Typeable
import System.IO
import Data.List (isPrefixOf)

lowerFirst :: String -> String
lowerFirst (x:xs) = toLower x : xs
lowerFirst [] = []

capitalize :: String -> String
capitalize (x:xs) = toUpper x : xs
capitalize [] = []

fieldName :: String -> String -> Name
fieldName typeName field = mkName $ lowerFirst typeName ++ capitalize field

-- Function to generate the model code as a string (not performing IO here)
generateModel :: String -> [(String, Name)] -> Q [Dec]
generateModel typeName fields = do
  let typeN = mkName typeName
      constructor = typeN

  fieldDecs <- mapM
    (\(fname, ftype) -> do
        let name' = fieldName typeName fname
        return (name', Bang NoSourceUnpackedness NoSourceStrictness, ConT ftype)
    ) fields

  let dataDef = DataD [] typeN [] Nothing
                  [RecC constructor fieldDecs]
                  [DerivClause Nothing [ConT ''Show, ConT ''Eq, ConT ''Typeable]]

  return [dataDef]

-- Function to generate the boilerplate file contents with the correct model code
generateFileContents :: String -> String -> String
generateFileContents typeName modelCode = unlines [
    "module " ++ typeName ++ " where",
    "",
    "import Data.Typeable",
    "import Prelude hiding (id)",
    "",
    modelCode
  ]

-- Custom pretty-printing to avoid fully qualified names
simplePpr :: Dec -> Doc
simplePpr (DataD _ tName _ _ [RecC _ fields] [DerivClause _ derived]) = do
  let typeStr = text (nameBase tName)
      fieldDocs = map (\(fName, _, ConT fType) ->
                        hcat [text (nameBase fName), text " :: ", text (nameBase fType)])
                      fields
      fieldStr = vcat (punctuate comma (map (\d -> hcat [text "  ", d]) fieldDocs))
      derivDocs = map (\(ConT d) -> text (nameBase d)) derived
      derivStr = hcat [text "deriving (", hcat (punctuate comma derivDocs), text ")"]

  -- Ensure `deriving` is on the same line as the closing brace
  hcat [text "data ", typeStr, text " = ", typeStr, text " {",
        fieldStr,
        text "} ", derivStr]  -- Deriving on the same line as the closing brace
simplePpr _ = empty


-- Helper function to write code to a file
writeModelToFile :: String -> String -> IO ()
writeModelToFile typeName _ = do
  let fileName = "src/" ++ typeName ++ ".hs"
  decs <- case typeName of
    "User" -> runQ $ generateModel "User" [("id", ''Int), ("name", ''String)]
    "Product" -> runQ $ generateModel "Product" [("sku", ''String), ("price", ''Double)]
    "Order" -> runQ $ generateModel "Order" [("orderId", ''Int), ("userId", ''Int), ("total", ''Double)]
    _ -> error $ "No model definition for " ++ typeName
  let modelCode = show (simplePpr (head decs))  -- Use custom pretty-printer
  writeFile fileName (generateFileContents typeName modelCode)

-- Debugging function for Model generation
printModel :: String -> Q [Dec] -> IO ()
printModel typeName modelQ = do
  decs <- runQ modelQ
  let modelCode = show (simplePpr (head decs))
  putStrLn $ "Generated model code for " ++ typeName ++ ":"
  putStrLn modelCode