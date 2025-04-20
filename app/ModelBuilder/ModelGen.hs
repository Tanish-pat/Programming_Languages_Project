{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module ModelGen (generateModel, generateAllModels, writeModelToFile) where

import GHC.Generics (Generic)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Name, VarStrictType)  -- Add this import
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
fieldName modelName field = mkName $ lowerFirst modelName ++ capitalize field

-- Core TH generator for a model
generateModel :: String -> [(String, Name)] -> Q [Dec]
generateModel modelName fields = do
  let typeN = mkName modelName
  fieldDecs <- mapM
    (\(fname, ftype) -> pure (fieldName modelName fname,
                              Bang NoSourceUnpackedness NoSourceStrictness,
                              ConT ftype)) fields
  let dataDef = DataD [] typeN [] Nothing
                  [RecC typeN fieldDecs]
                  [DerivClause Nothing [ConT ''Show, ConT ''Eq, ConT ''Typeable, ConT (mkName "GHC.Generics.Generic")]]
  return [dataDef]

-- TH generator for multiple models
generateAllModels :: [(String, [(String, Name)])] -> Q [Dec]
generateAllModels = fmap concat . mapM (uncurry generateModel)

-- Pretty-printer for generated models
simplePpr :: Dec -> String
simplePpr (DataD _ tName _ _ [RecC _ fields] [DerivClause _ derived]) =
  "data " ++ nameBase tName ++ " = " ++ nameBase tName ++ " {\n" ++
    concatMap (\(fName, _, ConT fType) ->
                "  " ++ nameBase fName ++ " :: " ++ nameBase fType ++ if lastField fName fields then "" else ",\n") fields ++
  "\n} deriving (" ++
    concat (punctuateComma (map (\(ConT d) -> nameBase d) derived)) ++ ")"
  where
    -- Check if it's the last field in the list
    lastField :: Name -> [VarStrictType] -> Bool
    lastField fName fields = fName == fst3 (last fields)

    -- Helper to access the first element of a 3-tuple
    fst3 :: (a, b, c) -> a
    fst3 (x, _, _) = x

    punctuateComma [] = []
    punctuateComma [x] = [x]
    punctuateComma (x:xs) = x : map (", " ++) xs
simplePpr _ = error "Unsupported declaration"

writeModelToFile :: String -> [(String, Name)] -> IO ()
writeModelToFile modelName fields = do
  decs <- runQ $ generateModel modelName fields
  let modelCode = simplePpr (head decs)
      fileContent = unlines
        [ "{-# LANGUAGE DeriveGeneric #-}"
        , ""
        , "module " ++ modelName ++ " where"
        , ""
        , "import Data.Typeable"
        , "import Prelude hiding (id)"
        , "import Data.Text (Text)"
        , "import GHC.Generics (Generic)"
        , "import Data.Aeson (ToJSON, FromJSON)"
        , ""
        , modelCode
        , ""
        , "instance ToJSON " ++ modelName
        , "instance FromJSON " ++ modelName
        ]
      targetDir = "generated/models"
      filePath = targetDir </> modelName ++ ".hs"
  createDirectoryIfMissing True targetDir
  writeFile filePath fileContent
