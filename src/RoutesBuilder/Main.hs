module Main where

import RouteGen
import RouteRegistry

main :: IO ()
main = do
  let outDir = "../../generated/routes"
  mapM_ (writeModule outDir) routeSpecs
