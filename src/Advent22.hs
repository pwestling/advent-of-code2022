{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Advent22 where

import System.CPUTime (getCPUTime)
import Control.Monad.IO.Class (liftIO)
import Util
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Char
import Debug.Trace

import qualified Data.Map as Map

data Cell = Open | Wall | Void deriving (Show, Eq, Ord)
data Walk = Go Int | L | R deriving (Show, Eq, Ord)
data Heading = N | E | S | W deriving (Show, Eq, Ord)
data Place = Place{ pos :: Point, heading :: Heading } deriving (Show, Eq, Ord)
data Maze = Maze{ cells :: Map.Map Point Cell, maxX :: Int, maxY :: Int, place :: Place } deriving (Show, Eq, Ord)

toCell :: Char -> Cell
toCell '#' = Wall
toCell '.' = Open
toCell ' ' = Void

toMaze :: [[Char]] -> Maze
toMaze s = result where
  toRow :: Int -> [Char] -> [(Point, Cell)]
  toRow y = map (\(x, c) -> (Point x y, toCell c)) . zip [0..]
  toRows :: [[Char]] -> [(Point, Cell)]
  toRows = concat . zipWith toRow [0..]
  rows = toRows s
  rowMap = Map.fromList rows
  startPoint = fst $ head $ filter (\(_,c) -> c /= Void) rows
  maxX = (maximum $ map (getX . fst) rows) + 1
  maxY = (maximum $ map (getY . fst) rows) + 1
  result = Maze rowMap maxX maxY (Place startPoint E)


parseMaze :: Parser Maze
parseMaze = do
  rows <- (many1 (oneOf " .#")) `sepEndBy` newline
  return $ toMaze rows

parseWalk :: Parser Walk
parseWalk = (char 'L' >> return L) <|> (char 'R' >> return R) <|> (Go <$> parseNumber)

parseWalks :: Parser [Walk]
parseWalks = many1 parseWalk

parseMazeAndWalks :: Parser (Maze, [Walk])
parseMazeAndWalks = do
  maze <- parseMaze
  newline
  walks <- parseWalks
  return (maze, walks)

getStep :: Heading -> Point
getStep N = Point 0 (-1)
getStep E = Point 1 0
getStep S = Point 0 1
getStep W = Point (-1) 0

performWalk :: Maze -> Walk -> Maze
performWalk maze R = maze { place = (maze.place) { heading = nextHeading } } where
  nextHeading = case (maze.place.heading) of
    N -> E
    E -> S
    S -> W
    W -> N
performWalk maze L = foldl performWalk maze [R,R,R]
performWalk maze (Go 0) = maze
performWalk maze (Go n) = result where
  step = getStep (maze.place.heading)
  newPoint = (\(Point x y) -> (Point (x `mod` maze.maxX) (y `mod` maze.maxY))) $ maze.place.pos <> step
  contents = Map.findWithDefault Void newPoint (maze.cells)
  newMaze = maze {place=(Place newPoint maze.place.heading)}
  result = case contents of
    Void -> performVoidWalk (maze.place.pos) newMaze (Go n)
    Wall -> maze
    Open -> performWalk newMaze (Go (n-1))

performVoidWalk :: Point -> Maze -> Walk -> Maze
performVoidWalk revert maze (Go 0) = maze
performVoidWalk revert maze (Go n) = result where
    step = getStep (maze.place.heading)
    newPoint = (\(Point x y) -> (Point (x `mod` maze.maxX) (y `mod` maze.maxY))) $ maze.place.pos <> step
    contents = Map.findWithDefault Void newPoint (maze.cells)
    newMaze = maze {place=(Place newPoint maze.place.heading)}
    result = case contents of
      Void -> performVoidWalk revert newMaze (Go n)
      Wall -> maze {place=(Place revert maze.place.heading)}
      Open -> performWalk newMaze (Go (n-1))


renderMap :: Maze -> String
renderMap maze = result where
  toRow y = map (\x -> toChar (Point x y)) [0..maze.maxX]
  toChar p = if maze.place.pos == p then
                case maze.place.heading of
                                N -> '^'
                                E -> '>'
                                S -> 'v'
                                W -> '<'
             else
                case Map.findWithDefault Void p (maze.cells) of
                    Void -> ' '
                    Wall -> '#'
                    Open -> '.'
  rows = map toRow [0..maze.maxY]
  result = unlines rows

headingToValue :: Heading -> Int
headingToValue E = 0
headingToValue S = 1
headingToValue W = 2
headingToValue N = 3

calcPassword :: Place -> Int
calcPassword place = result where
  x = (getX $ place.pos) + 1
  y = (getY $ place.pos) + 1
  headingVal = headingToValue $ place.heading
  result = 1000 * y + 4 * x + headingVal

main :: IO ()
main = do
  t1 <- liftIO getCPUTime
  s <- resource "password-maze"
  let entries = runParser (parseMazeAndWalks ) () "input" s
  let stuff = case entries of
          Left err -> error $ show err
          Right stuff -> stuff
  let (maze, walks) = stuff
  let results = (scanl performWalk maze walks)
--  mapM_ putStrLn $ fmap renderMap results
  let result = last results
  print $ calcPassword $ result.place

  t2 <- liftIO getCPUTime
  let diff = (fromIntegral (t2 - t1)) / (10^12)
  putStrLn $ "Computation time: " ++ show diff ++ " sec"