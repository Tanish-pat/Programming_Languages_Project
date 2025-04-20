module Main where

import RouteGen (writeModule)
import RouteRegistry (routeRegistry)

main :: IO ()
main = do
  let outDir = "../../generated/routes"
  mapM_ (writeModule outDir) routeRegistry
