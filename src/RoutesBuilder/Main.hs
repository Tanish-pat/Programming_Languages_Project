module Main where

import RouteGen (writeModule)
import RouteRegistry (routeRegistry)
import System.Directory (createDirectoryIfMissing)

main :: IO ()
main = do
  let outDir = "../../generated/routes"
  -- Create the directory if it doesn't exist
  createDirectoryIfMissing True outDir

  -- Write all route modules
  mapM_ (writeModule outDir) routeRegistry
