module DynamicComposer (composeFunctions, showPipelineFancy) where

import Types
import Colors

type Func = String -> String

composeFunctions :: [Func] -> Func
composeFunctions = foldr (.) id

showPipelineFancy :: Pipeline -> IO ()
showPipelineFancy (Pipeline pname steps) = do
    putStrLn $ green ("\n>> Pipeline: " ++ pname)
    mapM_ (\(i, code) -> putStrLn $ yellow ("   " ++ show i ++ ". " ++ code)) (zip [1..] steps)