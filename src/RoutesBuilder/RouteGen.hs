{-# LANGUAGE OverloadedStrings #-}

module RouteGen (writeModule) where

import           RouteRegistry
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import           System.Directory (createDirectoryIfMissing)
import           System.FilePath  ((</>))
import           Data.Char        (toLower)

writeModule :: FilePath -> RouteGroup -> IO ()
writeModule outDir (RouteGroup model routes) = do
  let mdl        = map toLower model
      moduleName = model ++ "Routes"
      filePath   = outDir </> moduleName ++ ".hs"

      header = T.unlines
        [ "module " <> T.pack moduleName
          <> " (" <> exportList routes <> ") where"
        , ""
        , "import Data.Text (Text, (<>), intercalate)"
        , "import qualified Data.Text as T"
        , ""
        , "basePath :: Text"
        , "basePath = \"/api/" <> T.pack mdl <> "\""
        , ""
        , "path :: [Text] -> Text"
        , "path segments = basePath <> \"/\" <> intercalate \"/\" segments"
        , ""
        , "query :: [(Text, Text)] -> Text"
        , "query [] = \"\""
        , "query ps = \"?\" <> intercalate \"&\" [k <> \"=\" <> v | (k, v) <- ps]"
        , ""
        ]

      body = T.unlines $ map (routeDecl mdl) routes

  createDirectoryIfMissing True outDir
  TIO.writeFile filePath (header <> body)

-- | Comma‑separated export list
exportList :: [RouteSpecExtended] -> T.Text
exportList rs = T.intercalate ", " (map (T.pack . functionName) rs)

routeDecl :: String -> RouteSpecExtended -> T.Text
routeDecl mdl (RouteSpecExtended fname _verb parts queries) =
  let segParams = [ v | Dynamic v <- parts ]
      args      = segParams ++ queries

      sig = T.pack fname <> " :: "
          <> if null args
               then "Text"
               else T.intercalate " -> " (replicate (length args) "Text") <> " -> Text"

      def = T.pack fname <> (if null args then "" else " " <> T.unwords (map T.pack args))
          <> " = " <> routeExpr mdl parts queries
  in T.unlines [sig, def]

routeExpr :: String -> [RoutePathPart] -> [String] -> T.Text
routeExpr mdl parts queries =
  let -- Build the path call
      segCodes = map partCode parts
      pathCode = T.concat ["path [", T.intercalate ", " segCodes, "]"]

      -- Build the query call if needed
      queryCode = if null queries
        then ""
        else
          T.concat
            [ " <> query ["
            , T.intercalate ", "
                [ T.concat ["(\"", T.pack q, "\", ", T.pack q, ")"]
                | q <- queries
                ]
            , "]"
            ]
  in T.concat [pathCode, queryCode]

-- | Render one path segment as Text of Haskell code
partCode :: RoutePathPart -> T.Text
partCode (Static s)  = T.concat ["\"", T.pack s, "\""]
partCode (Dynamic v) = T.pack v
