{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SnippetEvaluator (evaluateSnippet, compilePipeline) where

import Language.Haskell.Interpreter
import Types

evaluateSnippet :: Snippet -> IO (Either InterpreterError (String -> String))
evaluateSnippet code = runInterpreter $ do
    setImports ["Prelude", "Data.Char"]
    interpret code (as :: String -> String)

compilePipeline :: Pipeline -> IO [ (String -> String) ]
compilePipeline (Pipeline _ snippets) = mapM compile snippets
  where
    compile snip = do
        result <- evaluateSnippet snip
        case result of
            Left err -> error ("Runtime compile error: " ++ show err)
            Right f  -> return f