{-# LANGUAGE TemplateHaskell #-}

module Main where

import Colors
import Types
import Utils
import Templates
import DynamicComposer
import FileManager
import SnippetEvaluator
import THGenerated

import qualified Data.Map as Map
import Data.Char (toUpper)

-- Bring generated functions into scope
$(makeTransformations)

main :: IO ()
main = mainMenu

mainMenu :: IO ()
mainMenu = do
    divider
    putStrLn $ green "MetaPipeline: Runtime + Compile-Time Power"
    divider
    putStrLn $ blue "[1] Create New Pipeline"
    putStrLn $ blue "[2] Load Existing Pipeline"
    putStrLn $ blue "[3] View Built-in Templates"
    putStrLn $ blue "[4] Test Compile-Time Functions"
    putStrLn $ blue "[5] Exit"
    divider
    input <- prompt "> "
    case input of
        "1" -> createPipeline
        "2" -> loadPipelineFlow
        "3" -> viewTemplates >> mainMenu
        "4" -> demoTHGenerated >> mainMenu
        "5" -> putStrLn (green "Goodbye!")
        _   -> putStrLn (red "Invalid option.") >> mainMenu

createPipeline :: IO ()
createPipeline = do
    pname <- prompt "Enter pipeline name: "
    snippets <- collectSnippets [] 1
    let pipeline = Pipeline pname snippets
    savePipeline pipeline
    putStrLn (green "Pipeline saved successfully.")
    mainMenu

collectSnippets :: [Snippet] -> Int -> IO [Snippet]
collectSnippets acc n = do
    code <- prompt ("Enter snippet #" ++ show n ++ " (or type DONE): ")
    if code == "DONE"
      then return (reverse acc)
      else do
          res <- evaluateSnippet code
          case res of
              Left err -> putStrLn (red ("Error: " ++ show err)) >> collectSnippets acc n
              Right _  -> putStrLn (green "Snippet compiled!") >> collectSnippets (code:acc) (n + 1)

loadPipelineFlow :: IO ()
loadPipelineFlow = do
    pname <- prompt "Enter pipeline name to load: "
    mpipeline <- loadPipeline pname
    case mpipeline of
        Nothing -> putStrLn (red "Pipeline not found.") >> mainMenu
        Just pipeline -> pipelineMenu pipeline

pipelineMenu :: Pipeline -> IO ()
pipelineMenu pipeline = do
    divider
    showPipelineFancy pipeline
    divider
    putStrLn $ blue "[1] Apply Pipeline"
    putStrLn $ blue "[2] Return to Main Menu"
    input <- prompt "> "
    case input of
        "1" -> applyPipeline pipeline >> pipelineMenu pipeline
        "2" -> mainMenu
        _   -> putStrLn (red "Invalid option.") >> pipelineMenu pipeline

applyPipeline :: Pipeline -> IO ()
applyPipeline pipeline = do
    funcs <- compilePipeline pipeline
    let finalFunc = composeFunctions funcs
    input <- prompt "Enter input string: "
    putStrLn (yellow ("Output: " ++ finalFunc input))

viewTemplates :: IO ()
viewTemplates = do
    divider
    mapM_ (\(i, (desc, code)) -> putStrLn (yellow ("[" ++ show i ++ "] " ++ desc ++ ": " ++ code)))
          (zip [1..] templates)
    divider

demoTHGenerated :: IO ()
demoTHGenerated = do
    divider
    putStrLn $ green "Compile-Time Functions Demo:"
    putStrLn $ yellow ("reverseString \"MetaProgramming\" = " ++ reverseString "MetaProgramming")
    putStrLn $ yellow ("toUpperCase \"haskell\" = " ++ toUpperCase "haskell")
    putStrLn $ yellow ("addExclaim \"Wow\" = " ++ addExclaim "Wow")
    divider