module Colors where

green, red, yellow, blue, magenta :: String -> String

green s = "\x1b[32m" ++ s ++ "\x1b[0m"
red s   = "\x1b[31m" ++ s ++ "\x1b[0m"
yellow s = "\x1b[33m" ++ s ++ "\x1b[0m"
blue s  = "\x1b[34m" ++ s ++ "\x1b[0m"
magenta s = "\x1b[35m" ++ s ++ "\x1b[0m"