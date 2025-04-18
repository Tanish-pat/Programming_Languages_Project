{-# LANGUAGE OverloadedStrings #-}

module RouteGen where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import RouteRegistry
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing)

-- List of endpoints that require a parameter
dynamicRoutes :: [String]
dynamicRoutes = ["getById", "update", "delete"]

-- Generate a module for each RouteSpec
writeModule :: FilePath -> RouteSpec -> IO ()
writeModule outDir (RouteSpec model paths funcs) = do
  let moduleName = model ++ "Routes"
      fileName = outDir </> moduleName ++ ".hs"
      header = T.unlines
        [ T.pack $ "module " ++ moduleName ++ " (" ++ exports funcs ++ ") where"
        , ""
        , "import Data.Text (Text, (<>))"
        , ""
        ]

      body = T.unlines $ zipWith (generateFunction model) paths funcs

  createDirectoryIfMissing True outDir
  TIO.writeFile fileName (header <> body)

-- Format exported function list
exports :: [String] -> String
exports = concat . zipWith (\i name -> if i == 0 then name else ", " ++ name) [0..]

-- Generate function definitions
generateFunction :: String -> String -> String -> T.Text
generateFunction model path fname
  | path `elem` dynamicRoutes =
      T.unlines
        [ T.pack $ fname ++ " :: Text -> Text"
        , T.pack $ fname ++ " x = " ++ show ("/" ++ mapLower model ++ "/" ++ path ++ "/") ++ " <> x"
        ]
  | otherwise =
      T.unlines
        [ T.pack $ fname ++ " :: Text"
        , T.pack $ fname ++ " = " ++ show ("/" ++ mapLower model ++ "/" ++ path)
        ]

-- Lowercase the first character of a string
mapLower :: String -> String
mapLower [] = []
mapLower (x:xs) = toEnum (fromEnum x + 32) : xs