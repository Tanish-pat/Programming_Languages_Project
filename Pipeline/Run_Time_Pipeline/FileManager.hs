module FileManager (savePipeline, loadPipeline) where

import System.Directory
import System.IO
import Types

savePipeline :: Pipeline -> IO ()
savePipeline pipeline = writeFile (name pipeline ++ ".pipeline") (show pipeline)

loadPipeline :: String -> IO (Maybe Pipeline)
loadPipeline pname = do
    exists <- doesFileExist (pname ++ ".pipeline")
    if exists
        then do
            content <- readFile (pname ++ ".pipeline")
            return (Just (read content))
        else return Nothing