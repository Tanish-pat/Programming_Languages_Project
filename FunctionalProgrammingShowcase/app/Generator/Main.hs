{-# LANGUAGE ExtendedDefaultRules #-}

module Main where

import System.IO
import System.Directory
import Data.Char (toUpper)
import Data.List (intercalate)
import Language.Haskell.Interpreter

-- Capitalize the first letter of a string
capitalize :: String -> String
capitalize []     = []
capitalize (x:xs) = toUpper x : xs

-- Generate the function definition string
generateFunction :: String -> [String] -> String -> String
generateFunction fname args body =
  let
    inputTypes    = replicate (length args) "Int"
    fullSignature = intercalate " -> " (inputTypes ++ ["Int"])
  in
    fname ++ " :: " ++ fullSignature ++ "\n"
    ++ fname ++ " " ++ unwords args ++ " = " ++ body ++ "\n\n"

-- Generate the entire module string
generateModule :: String -> [(String, [String], String)] -> String
generateModule modName functions =
  "module " ++ modName ++ " where\n\n"
  ++ concatMap (\(f,a,b) -> generateFunction f a b) functions

-- Dynamically test a generated module using hint
testGeneratedModule :: String -> [(String, [String])] -> IO ()
testGeneratedModule modName fnamesWithArgs = do
  r <- runInterpreter $ do
    set [searchPath := ["app/Generator"]]
    loadModules ["app/Generator/" ++ modName ++ ".hs"]
    setTopLevelModules [capitalize modName]

    liftIO $ putStrLn "Testing the generated module interactively..."

    mapM_ (\(fname, argsList) -> do
        liftIO $ putStrLn $ "Function: " ++ fname
        liftIO $ putStrLn $ "Expected " ++ show (length argsList) ++ " arguments."
        liftIO $ putStrLn "Enter arguments (space-separated):"
        argsLine <- liftIO getLine
        let args = words argsLine
        if length args /= length argsList then
          liftIO $ putStrLn "Error: Incorrect number of arguments."
        else do
          let expr = unwords (fname : args)
          result <- interpret expr (as :: Int)
          liftIO $ putStrLn $ fname ++ " " ++ unwords args ++ " = " ++ show result
      ) fnamesWithArgs

  case r of
    Left err -> putStrLn $ "Runtime error: " ++ show err
    Right _  -> return ()

-- Main program logic
main :: IO ()
main = do
  hSetBuffering stdout LineBuffering

  -- Solicit module name
  putStrLn "Enter Module Name:"
  modName <- capitalize <$> getLine

  -- Solicit number of functions
  putStrLn "How many functions to create?"
  n <- readLn :: IO Int

  -- Gather each function’s metadata
  functions <- mapM
    (\i -> do
       putStrLn $ "Function " ++ show (i + 1)
       putStrLn "Enter function name:"
       fname <- getLine
       putStrLn "Enter argument names (space-separated):"
       args <- words <$> getLine
       putStrLn "Enter function body (in terms of arguments):"
       body <- getLine
       pure (fname, args, body)
    )
    [0 .. n - 1]

  -- Assemble and write module file
  let content  = generateModule modName functions
      fileName = "app/Generator/" ++ modName ++ ".hs"
  writeFile fileName content

  putStrLn $ "Module " ++ fileName ++ " generated successfully."

  -- Now, immediately test the generated module
  putStrLn "Testing the generated module..."
  testGeneratedModule modName (map (\(fname,args,_) -> (fname,args)) functions)










-- {-# LANGUAGE ExtendedDefaultRules #-}

-- module Main where

-- import System.IO
-- import System.Directory
-- import Data.Char (toUpper)
-- import Data.List (intercalate)
-- import Language.Haskell.Interpreter

-- -- Capitalize the first letter of a string
-- capitalize :: String -> String
-- capitalize []     = []
-- capitalize (x:xs) = toUpper x : xs

-- -- Generate the function definition string
-- generateFunction :: String -> [String] -> String -> String
-- generateFunction fname args body =
--   let
--     inputTypes    = replicate (length args) "Int"
--     fullSignature = intercalate " -> " (inputTypes ++ ["Int"])
--   in
--     fname ++ " :: " ++ fullSignature ++ "\n"
--     ++ fname ++ " " ++ unwords args ++ " = " ++ body ++ "\n\n"

-- -- Generate the entire module string
-- generateModule :: String -> [(String, [String], String)] -> String
-- generateModule modName functions =
--   "module " ++ modName ++ " where\n\n"
--   ++ concatMap (\(f,a,b) -> generateFunction f a b) functions

-- -- Dynamically test a generated module using hint
-- testGeneratedModule :: String -> [String] -> IO ()
-- testGeneratedModule modName fnames = do
--   r <- runInterpreter $ do
--     set [searchPath := ["app/Generator"]]
--     loadModules ["app/Generator/" ++ modName ++ ".hs"]
--     setTopLevelModules [capitalize modName]

--     -- Run and print result of each function dynamically
--     mapM_ (\fname -> do
--         result <- interpret (fname ++ " 1 2") (as :: Int)
--         liftIO $ putStrLn $ fname ++ " 1 2 = " ++ show result
--       ) fnames

--   case r of
--     Left err -> putStrLn $ "Runtime error: " ++ show err
--     Right _  -> return ()

-- -- Main program logic
-- main :: IO ()
-- main = do
--   hSetBuffering stdout LineBuffering

--   -- Solicit module name
--   putStrLn "Enter Module Name:"
--   modName <- capitalize <$> getLine

--   -- Solicit number of functions
--   putStrLn "How many functions to create?"
--   n <- readLn :: IO Int

--   -- Gather each function’s metadata
--   functions <- mapM
--     (\i -> do
--        putStrLn $ "Function " ++ show (i + 1)
--        putStrLn "Enter function name:"
--        fname <- getLine
--        putStrLn "Enter argument names (space-separated):"
--        args <- words <$> getLine
--        putStrLn "Enter function body (in terms of arguments):"
--        body <- getLine
--        pure (fname, args, body)
--     )
--     [0 .. n - 1]

--   -- Assemble and write module file
--   let content  = generateModule modName functions
--       fileName = "app/Generator/" ++ modName ++ ".hs"
--   writeFile fileName content

--   putStrLn $ "Module " ++ fileName ++ " generated successfully."

--   -- Now, immediately test the generated module
--   putStrLn "Testing the generated module..."
--   testGeneratedModule modName (map (\(fname,_,_) -> fname) functions)