{-# LANGUAGE TemplateHaskell #-}

module ModelGen (generateModel, generateAllModels, writeModelToFile) where

import Language.Haskell.TH
import Language.Haskell.TH.PprLib
import Data.Char (toLower, toUpper)
import Data.Typeable
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

-- Lowercase first character
lowerFirst :: String -> String
lowerFirst (x:xs) = toLower x : xs
lowerFirst [] = []

-- Capitalize first character
capitalize :: String -> String
capitalize (x:xs) = toUpper x : xs
capitalize [] = []

-- Generate field name: User + id → userId
fieldName :: String -> String -> Name
fieldName typeName field = mkName $ lowerFirst typeName ++ capitalize field

-- Core TH generator for a model
generateModel :: String -> [(String, Name)] -> Q [Dec]
generateModel typeName fields = do
  let typeN = mkName typeName
  fieldDecs <- mapM
    (\(fname, ftype) -> pure (fieldName typeName fname,
                              Bang NoSourceUnpackedness NoSourceStrictness,
                              ConT ftype)) fields
  let dataDef = DataD [] typeN [] Nothing
                  [RecC typeN fieldDecs]
                  [DerivClause Nothing [ConT ''Show, ConT ''Eq, ConT ''Typeable]]
  return [dataDef]

-- TH generator for multiple models
generateAllModels :: [(String, [(String, Name)])] -> Q [Dec]
generateAllModels = fmap concat . mapM (uncurry generateModel)

-- Pretty-printer for generated models
simplePpr :: Dec -> String
simplePpr (DataD _ tName _ _ [RecC _ fields] [DerivClause _ derived]) =
  "data " ++ nameBase tName ++ " = " ++ nameBase tName ++ " {\n" ++
    concatMap (\(fName, _, ConT fType) ->
                "  " ++ nameBase fName ++ " :: " ++ nameBase fType ++ ",\n") fields ++
  "} deriving (" ++
    concat (punctuateComma (map (\(ConT d) -> nameBase d) derived)) ++ ")"
  where
    punctuateComma [] = []
    punctuateComma [x] = [x]
    punctuateComma (x:xs) = x : map (", " ++) xs
simplePpr _ = error "Unsupported declaration"

-- Write a model to a file under ./generated/
writeModelToFile :: String -> [(String, Name)] -> IO ()
writeModelToFile typeName fields = do
  decs <- runQ $ generateModel typeName fields
  let modelCode = simplePpr (head decs)
      fileContent = unlines
        [ "module " ++ typeName ++ " where"
        , ""
        , "import Data.Typeable"
        , "import Prelude hiding (id)"
        , ""
        , modelCode
        ]
      targetDir = "generated"
      filePath = targetDir </> typeName ++ ".hs"
  createDirectoryIfMissing True targetDir
  writeFile filePath fileContent
