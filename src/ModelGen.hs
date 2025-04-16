{-# LANGUAGE TemplateHaskell #-}

module ModelGen (generateModel) where

import Language.Haskell.TH
import Data.Char (toLower, toUpper)
import Data.Typeable

lowerFirst :: String -> String
lowerFirst (x:xs) = toLower x : xs
lowerFirst [] = []

capitalize :: String -> String
capitalize (x:xs) = toUpper x : xs
capitalize [] = []

fieldName :: String -> String -> Name
fieldName typeName field = mkName $ lowerFirst typeName ++ capitalize field

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