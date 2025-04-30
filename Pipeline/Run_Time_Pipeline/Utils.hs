module Utils where

import Colors
import System.IO (hFlush, stdout)

prompt :: String -> IO String
prompt msg = putStr (blue msg) >> hFlush stdout >> getLine

divider :: IO ()
divider = putStrLn (magenta (replicate 40 '-'))