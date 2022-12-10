module Util(resource, end) where

import System.Environment (getExecutablePath)
import System.FilePath (takeDirectory)
import qualified Paths_advent_of_code2022 as Paths
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Char
import Control.Monad

resource :: String -> IO String
resource name = do
    filePath <- Paths.getDataFileName ("resources/" ++ name)
    readFile filePath


end :: Parser ()
end = void newline <|> eof