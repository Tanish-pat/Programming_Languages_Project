module Main where

import Immutability (immutableList, addElement, multiplyElements, filterElementsGreater, filterElementsLesser)
import System.IO (hFlush, stdout)
import qualified Data.ByteString.Lazy as B
import Data.Aeson (encode, eitherDecode)
import System.IO (withFile, IOMode(..))
import Data.Maybe (fromMaybe)
import System.Directory (doesFileExist)

-- Path to the state.json file in the Immutability folder
stateFilePath :: FilePath
stateFilePath = "app/Immutability/state.json"

-- Function to read the list from state.json
loadState :: IO [Int]
loadState = do
    fileExists <- doesFileExist stateFilePath
    if fileExists
        then withFile stateFilePath ReadMode $ \handle -> do
            content <- B.hGetContents handle
            let decodedState = eitherDecode content :: Either String [Int]
            return $ fromMaybe immutableList (either (const Nothing) Just decodedState)
        else return immutableList

-- Function to save the list to state.json
saveState :: [Int] -> IO ()
saveState newState = withFile stateFilePath WriteMode $ \handle -> do
    let encodedState = encode newState
    B.hPut handle encodedState
    putStrLn "State saved to state.json."

-- Function to handle adding an element to the list
addElementCLI :: IO [Int]
addElementCLI = do
    putStr "Enter an integer to add to the list: "
    hFlush stdout
    input <- getLine
    let newList = addElement (read input :: Int)
    putStrLn $ "Updated List: " ++ show newList
    putStrLn "Do you want to save the updated state? (yes/no)"
    hFlush stdout
    saveResponse <- getLine
    if saveResponse == "yes" then saveState newList else putStrLn "State not saved."
    return newList

-- Function to handle multiplying elements of the list
multiplyElementsCLI :: IO [Int]
multiplyElementsCLI = do
    putStr "Enter a factor to multiply the elements: "
    hFlush stdout
    input <- getLine
    let newList = multiplyElements (read input :: Int)
    putStrLn $ "Updated List after multiplying by factor: " ++ show newList
    putStrLn "Do you want to save the updated state? (yes/no)"
    hFlush stdout
    saveResponse <- getLine
    if saveResponse == "yes" then saveState newList else putStrLn "State not saved."
    return newList

-- Function to handle filtering elements greater than a threshold
filterElementsGreaterThanCLI :: IO [Int]
filterElementsGreaterThanCLI = do
    putStr "Enter a threshold to filter elements: "
    hFlush stdout
    input <- getLine
    let newList = filterElementsGreater (read input :: Int)
    putStrLn $ "Updated List after filtering: " ++ show newList
    putStrLn "Do you want to save the updated state? (yes/no)"
    hFlush stdout
    saveResponse <- getLine
    if saveResponse == "yes" then saveState newList else putStrLn "State not saved."
    return newList

-- Function to handle filtering elements greater than a threshold
filterElementsLesserThanCLI :: IO [Int]
filterElementsLesserThanCLI = do
    putStr "Enter a threshold to filter elements: "
    hFlush stdout
    input <- getLine
    let newList = filterElementsLesser (read input :: Int)
    putStrLn $ "Updated List after filtering: " ++ show newList
    putStrLn "Do you want to save the updated state? (yes/no)"
    hFlush stdout
    saveResponse <- getLine
    if saveResponse == "yes" then saveState newList else putStrLn "State not saved."
    return newList

-- Function to display the menu
displayMenu :: IO Int
displayMenu = do
    putStrLn "\nSelect an operation:"
    putStrLn "1. Add an element"
    putStrLn "2. Multiply elements"
    putStrLn "3. Filter elements By Greater"
    putStrLn "4. Filter elements By Lesser"
    putStrLn "5. Exit"
    putStr "Enter your choice (1-5): "
    hFlush stdout
    input <- getLine
    return (read input :: Int)

-- Main program logic
main :: IO ()
main = do
    putStrLn "Do you want to load the previous state? (yes/no)"
    hFlush stdout
    response <- getLine
    currentState <- if response == "yes" then loadState else return immutableList
    -- CLI loop
    let loop state = do
            choice <- displayMenu
            case choice of
                1 -> do
                    newState <- addElementCLI
                    loop newState
                2 -> do
                    newState <- multiplyElementsCLI
                    loop newState
                3 -> do
                    newState <- filterElementsGreaterThanCLI
                    loop newState
                4 -> do
                    newState <- filterElementsLesserThanCLI
                    loop newState
                5 -> putStrLn "Exiting program."
                _ -> do
                    putStrLn "Invalid choice. Please select again."
                    loop state

    -- Start the loop
    loop currentState