module Util(resource) where

import System.Environment (getExecutablePath)
import System.FilePath (takeDirectory)
import qualified Paths_advent_of_code2022 as Paths

resource :: String -> IO String
resource name = do
    filePath <- Paths.getDataFileName ("resources/" ++ name)
    readFile filePath