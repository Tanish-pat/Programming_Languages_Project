{-# LANGUAGE TemplateHaskell #-}

module RoutesBuilder.Main where

import RouteGen (writeModule)
import RouteRegistry (routeRegistry)

main :: IO ()
main = do
  let outDir = "generated/routes"
  putStrLn "Generating Haskell files for routes into the generated/routes folder..."
  mapM_ (writeModule outDir) routeRegistry
  putStrLn "Routes generation complete."