module Util where

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

data Point = Point Int Int deriving (Show, Eq, Ord)

goUp :: Point -> Point
goUp (Point x y) = Point x (y + 1)

goDown :: Point -> Point
goDown (Point x y) = Point x (y - 1)

goLeft :: Point -> Point
goLeft (Point x y) = Point (x - 1) y

goRight :: Point -> Point
goRight (Point x y) = Point (x + 1) y

distance :: Point -> Point -> Int
distance (Point x1 y1) (Point x2 y2) = abs (x1 - x2) + abs (y1 - y2)

grabLeft :: Either a b -> a
grabLeft (Left x) = x
grabLeft (Right _) = error "fromLeft: Right"

grabRight :: Either a b -> b
grabRight (Right x) = x
grabRight (Left _) = error "fromRight: Left"