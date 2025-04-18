module RouteRegistry where

data RouteSpec = RouteSpec
  { modelName     :: String
  , routePaths    :: [String]
  , functionNames :: [String]
  }

-- NOTE: Make sure both lists have the same length
routeSpecs :: [RouteSpec]
routeSpecs =
  [ RouteSpec "User"
      ["getAll", "getById", "create", "update", "delete"]
      ["getAllUsers", "getById", "createNew", "updateById", "deleteById"]
  , RouteSpec "Product"
      ["getAll", "getById", "create", "update", "delete"]
      ["getAllProducts", "getById", "createNew", "updateById", "deleteById"]
  ]