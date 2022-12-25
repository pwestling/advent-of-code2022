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
import Data.Char

import qualified Data.Map as Map

data Cell = Open | Wall | Void deriving (Show, Eq, Ord)
data Walk = Go Int | L | R deriving (Show, Eq, Ord)
data Heading = N | E | S | W deriving (Show, Eq, Ord)
data Place = Place{ pos :: Point, heading :: Heading } deriving (Show, Eq, Ord)
data Maze = Maze{ cells :: Map.Map Point Cell, maxX :: Int, maxY :: Int, place :: Place, voidLinks :: Map.Map Place Place} deriving (Show, Eq, Ord)

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
  result = Maze rowMap maxX maxY (Place startPoint E) Map.empty


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
  newPoint = maze.place.pos <> step
  newPlace = if Map.member maze.place maze.voidLinks then (maze.voidLinks Map.! maze.place) else Place newPoint (maze.place.heading)
  contents = Map.findWithDefault Void (newPlace.pos) (maze.cells)
  newMaze = maze {place=newPlace}
  result = case contents of
    Void -> error $ "Void occurred at " ++ show newPlace ++ " stepping from " ++ show (maze.place) ++ " : " ++ show maze.voidLinks
    Wall -> maze
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
             else if p `elem` (fmap (.pos) (fmap fst (Map.toList maze.voidLinks))) then '*'
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

intDir :: Int -> Int
intDir i = if i > 0 then 1 else if i < 0 then -1 else 0

byOne :: Int -> Int -> [Int]
byOne start end = if isZero then [start] else result where
  isZero = start - end == 0
  dir = intDir $ end - start
  result = [start, start + dir .. end]

opposite :: Heading -> Heading
opposite N = S
opposite E = W
opposite S = N
opposite W = E

sew :: Side -> Side -> Maze  -> Maze
sew  (Side h1 p1 p2) (Side h2 p3 p4) maze = result where
  rowOne = [Point x y | x <- (getX p1 `byOne` getX p2), y <- (getY p1 `byOne` getY p2)]
  rowTwo = [Point x y | x <- (getX p3 `byOne` getX p4), y <- (getY p3 `byOne` getY p4)]
  enumeratedPlaces1 = fmap (\p -> Place p h1) rowOne
  enumeratedPlaces2 = fmap (\p -> Place p (opposite h2)) rowTwo

  enumeratedPlaces3 = fmap (\p -> Place p (opposite h1)) rowOne
  enumeratedPlaces4 = fmap (\p -> Place p ( h2)) rowTwo

  newVoidLinks = Map.fromList $ (zip enumeratedPlaces1 enumeratedPlaces2) ++ (zip enumeratedPlaces4 enumeratedPlaces3)
  result = maze { voidLinks = Map.union newVoidLinks (maze.voidLinks) }

apL :: [(a -> a)] -> a -> a
apL [] a = a
apL (f:fs) a = apL fs (f a)


data Side = Side Heading Point Point deriving (Show, Eq)
data Square = Square { n :: Side, e :: Side, s :: Side, w :: Side } deriving (Show, Eq)

reverseSide :: Side -> Side
reverseSide (Side h p1 p2) = Side (h) p2 p1

createSideRanges :: Int -> Point -> Square
createSideRanges size (Point x y) = Square n e s w where
    sz = size - 1
    n = Side N (Point (x) (y)) (Point (x+sz) (y))
    e = Side E (Point (x+sz) (y)) (Point (x+sz) (y+sz))
    s = Side S (Point (x) (y+sz)) (Point (x+sz) (y+sz))
    w = Side W (Point (x) (y)) (Point (x) (y+sz))




main :: IO ()
main = do
  t1 <- liftIO getCPUTime
  s <- resource "password-maze"
  let entries = runParser (parseMazeAndWalks ) () "input" s
  let stuff = case entries of
          Left err -> error $ show err
          Right stuff -> stuff
  let (maze, walks) = stuff

  let sqSize = 50
  let createSide = createSideRanges sqSize
  let side1 = createSide (Point 50 0)
  let side2 = createSide (Point 100 0)
  let side3 = createSide (Point 50 50)
  let side4 = createSide (Point 0 100)
  let side5 = createSide (Point 50 100)
  let side6 = createSide (Point 0 150)
  print $ side4.e
  let sewnMaze = apL [
           sew side1.n (side6.w),
           sew side2.n (side6.s),
           sew side1.w (reverseSide side4.w),
           sew side2.e (reverseSide side5.e),
           sew side3.e (side2.s),
           sew side3.w (side4.n),
           sew side5.s (side6.e)
          ] maze
  print "Computing..."
  putStrLn $ renderMap sewnMaze
  let results = (scanl performWalk sewnMaze walks)
  mapM_ putStrLn $ fmap (renderMap) [last results]
  let result = last results
  print $ calcPassword $ result.place

  t2 <- liftIO getCPUTime
  let diff = (fromIntegral (t2 - t1)) / (10^12)
  putStrLn $ "Computation time: " ++ show diff ++ " sec"